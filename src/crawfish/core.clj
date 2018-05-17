(ns crawfish.core
  (:require
   [clojure.core.async :as async :refer [put! >! >!! <! <!! go go-loop chan]]
   [clojure.inspector :as inspect]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [net.cgrand.enlive-html :as enlive]
   [org.httpkit.client :as kit])
  (:import
   [java.util.concurrent ConcurrentLinkedQueue])
  (:gen-class))

;; REPL use only
(comment
  (use 'clojure.repl 'clojure.pprint)
  (def monzo "https://monzo.com")
  (def clojars "https://clojars.org/")

  )

;; # Utils

(def logger (agent nil))
(def levels {:debug 0 :info 1 :warn 2 :error 3 :fatal 4})
(def ^:dynamic *log-level* :warn)

(defn log [level & msgs]
  (when (>= (levels level 0) (levels *log-level* 2))
    (send logger (fn [_] (apply println msgs)))))

(defn mk-ex
  [condition val]
  (when-not (condition val)
    (throw (ex-info "failed" {:condition condition
                              :value     val
                              :type      type}))))

;; # Parsing

;; TODO: strip query parameters from URLs

(defn page-internal?
  [s]
  (str/starts-with? s "#"))

(defn absolute?
  [s]
  ;; TODO: corner cases?
  ;; TODO: identify urls of the form //cdn.x.y
  #_ (re-find #"http" s)
  (str/starts-with? s "http"))

(defn external?
  [site-root]
  (fn [s]
    (and (absolute? s)
         (not (str/starts-with? s site-root)))))

(defn mailto?
  [s]
  (str/starts-with? s "mailto"))

(defn tel?
  [s]
  (str/starts-with? s "tel"))

;; TODO: update this 2018-05-17 10:22
(defn ignore?
  [s]
  (or (absolute? s)
      (mailto? s)
      (tel? s)
      (page-internal? s)))

;; TODO: rename -> begin-slash?
(defn has-slash?
  [s]
  (prn :has-slash? s)
  (doto (str/starts-with? s "/")
    prn))

(defn end-slash?
  [s]
  (str/ends-with? s "/"))

(defn remove-extra-slashes
  [s]
  (str/replace s #"(?<![:/])/+" "/"))

(defn absolutise [root]
  (fn [url]
    (remove-extra-slashes
     (if (absolute? url)
       url
       (str root "/" url)))))

(defn strip-query-params
  [s]
  (str/replace s #"(\?).*" ""))

(def href-sel
  #_ (enlive/attr? :href)
  ;; OR
  [:a (enlive/attr? :href)])

(defn xform-html
  "Takes a byte stream representing an HTML page and a transducer to apply to the sequence of URLs found.
  Returns a set of outgoing links."
  [byte-stream xform]
  (try (-> byte-stream
           enlive/html-resource
           (enlive/select [href-sel])
           (->> (map #(get-in % [:attrs :href]))
                (into #{} xform)))))

(defn outgoing-links
  "Takes a URL and a site-root, and returns a set of outgoing links."
  [url site-root]
  ;; TODO: experiment with callback API
  (-> @(kit/get url {:as :stream})
      :body
      ;; TODO: use ignore? as predicate
      (xform-html (comp (remove absolute?)
                        (remove mailto?)
                        (remove tel?)
                        (remove page-internal?)
                        (map strip-query-params)
                        (map (absolutise site-root))))))

;; # Hierarchical (tree) representation

(def sep #"/")

;; (def url "https://daveduthie.github.io/static/images/favicon.png")
;; (def url2 "https://daveduthie.github.io/static/images/foo.png")
;; (def url3 "https://daveduthie.github.io/articles/yo-ho-ho")

;; (into []
;;       (comp (remove empty?)
;;             (remove protocol?))
;;       (str/split url sep'))

;; TODO: interpret relative paths in segments ("/../")

(defn tokenise
  [url]
  (into []
        (comp (remove empty?) (remove ignore?))
        (str/split url sep)))

(defn deep-merge
  [m url]
  (let [path (tokenise url)]
    (update-in m path #(or % {}))))

(defn ->tree
  [links]
  (reduce deep-merge {} links))

;; # Control

(defn re-queue
  [returns work-q seen]
  (go-loop []
    (let [url (<! returns)]
      (assert url)
      (dosync (commute seen conj url))
      (.add work-q url) ; don't respect back-pressure here (need growing buffer *somewhere*)
      (recur))))

(defn wait-for-ack
  [ack seen]
  (let [gate (promise)
        _    (go-loop [i 1]
               (log :debug :waiting-for-ack i)
               (<! ack)
               (if (= i (count @seen))
                 (do (log :debug i :ack/done (count @seen))
                     (deliver gate :acks-complete!))
                 (do (log :warn i :ack/< (count @seen))
                     (recur (inc i)))))]
    gate))

;; # Workers

(defn proc
  [site-root work-q returns ack seen]
  (async/thread ; will block on I/O
    (loop []
      (when-let [url (.poll work-q)]
        (let [_    (log :debug :proc/got url)
              urls (->> (outgoing-links url site-root)
                        (remove (conj @seen url)))]

          ;; (dosync (commute seen into urls)) ; moved this logic to control
          (log :debug :proc/attempt-to-return url "->" (count urls))
          ;; (async/onto-chan returns urls false) ; TODO: bring me back
          (doseq [u urls] (>!! returns u))
          ;; Confirm url has been processed
          (>!! ack url)))
      (recur))))

;; # Wiring

(defn returns-xform
  "Prevents URLs which have already been seen from being put onto the returns chan
  and registers each new URL as it is encountered."
  [seen]
  (comp (distinct)
        (map (fn [x]
               (dosync (commute seen conj))
               (log :info :<-----------returns x)
               x))))

(defn process-all
  [site-root n]
  (let [ seen    (ref #{})
        work-q  (ConcurrentLinkedQueue.)
        #_      (chan 10 (map (fn [x] (log :info :----------->work-q x) x)))
        returns (chan 1 (returns-xform seen))
        ack     (chan 1 (map (fn [x] (log :info :<-----------ack x) x)))]
    (dosync (commute seen conj site-root))
    (.add work-q site-root)
    (re-queue returns work-q seen)
    (dotimes [i n]
      (proc site-root work-q returns ack seen))
    (prn @(wait-for-ack ack seen))
    @seen))

;; # Printing

(defn print-dir
  [from tos]
  (println from " => ")
  (println ".")
  (doseq [t (butlast tos)]
    (println "├──" t))
  (println "└──" (last tos)))


#_
(inspect/inspect-tree (->tree (process-all clojars 10)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
