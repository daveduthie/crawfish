(ns crawfish.core
  (:require
   [org.httpkit.client :as kit]
   [net.cgrand.enlive-html :as enlive]
   [clojure.string :as str]
   [clojure.core.async :as async
    :refer [put! >! >!! <! <!! go go-loop chan]]
   [clojure.java.io :as io])
  (:import
   [java.util.concurrent ConcurrentLinkedQueue])
  (:gen-class))

;; REPL use only
(comment
  (use 'clojure.repl 'clojure.pprint)
  (def monzo "https://monzo.com")
  (defonce fetched-monzo (:body @(kit/get monzo)))
  (def pages "https://daveduthie.github.io")
  (defonce fetched-pages (:body @(kit/get monzo)))

  )

(def logger (agent nil))

(defn log [& msgs]
  (send logger (fn [_] (apply println msgs))))

(defn mk-ex
  [condition val]
  (when-not (condition val)
    (throw (ex-info "failed" {:conition condition
                              :value    val
                              :type     type}))))

;; --------------------------------------------------------------------

#_
(defn string->enlive
  "Transforms a stringified HTML page into Enlive's internal representation."
  [s]
  (mk-ex string? s)
  (enlive/html-resource
   (io/input-stream (.getBytes s))))

;; --------------------------------------------------------------------

(defn relative?
  [s]
  ;; TODO: write me 2018-05-16 13:20
  )

(defn page-internal?
  [s]
  (str/starts-with? s "#"))

(defn absolute?
  [s]
  ;; TODO: corner cases?
  #_ (re-find #"http" s)
  (str/starts-with? s "http"))

(defn mailto?
  [s]
  (str/starts-with? s "mailto"))

(defn tel?
  [s]
  (str/starts-with? s "tel"))

(defn ignore?
  [s]
  (or (absolute? s)
      (mailto? s)
      (tel? s)
      (page-internal? s)))

;; --------------------------------------------------------------------

(defn has-slash?
  [s]
  (str/starts-with? s "/"))

(defn absolutise [root]
  (fn [url]
    (str root (when-not (has-slash? url) "/") url)))

(def href-sel
  ;; (enlive/attr? :href)
  ;; OR
  [:a (enlive/attr? :href)]
  )

(defn xform-html
  "Takes a byte stream representing an HTML page and a transducer to apply to URLs found.
  Returns a set of outgoing links."
  [byte-stream xform]
  (try (-> byte-stream
           enlive/html-resource
           (enlive/select [href-sel])
           (->> (map #(get-in % [:attrs :href]))
                (into #{} xform)))))

#_
(xform-html fetched-monzo (comp (remove absolute?)
                                (remove mailto?)
                                (remove tel?)
                                (map (absolutise monzo))))

(defn outgoing-links
  "Takes a URL and a site-root, and returns a set of outgoing links."
  [url site-root]
  ;; TODO: experiment with callback API
  (-> @(kit/get url {:as :stream})
      :body
      (xform-html (comp (remove absolute?)
                        (remove mailto?)
                        (remove tel?)
                        (remove page-internal?)
                        (map (absolutise site-root))))))

#_
(outgoing-links pages pages)

#_
(kit/get pages
         {:timeout 1000}
         (fn [{:keys [body]}]
           (if body
             (prn ::outgoing>> (outgoing-links body pages))
             (prn ::BORK))))

;; (defn enlive-fetch-url [url]
;;   (enlive/html-resource (java.net.URL. url)))

;; --------------------------------------------------------------------

(def sep #"/")

;; (def url "https://daveduthie.github.io/static/images/favicon.png")
;; (def url2 "https://daveduthie.github.io/static/images/foo.png")
;; (def url3 "https://daveduthie.github.io/articles/yo-ho-ho")

;; (into []
;;       (comp (remove empty?)
;;             (remove protocol?))
;;       (str/split url sep'))

(defn tokenise
  [url]
  (into [] (comp (remove empty?)
                 (remove ignore?))
        (str/split url sep)))

;; (tokenise url)

(defn deep-merge
  [m url]
  (let [path (tokenise url)]
    ;; (prn :path path)
    ;; (pprint m)
    (update-in m path #(or % {}))))

(defn ->tree
  [links]
  (reduce deep-merge {} links))

;; Control ------------------------------------------------------------

(defn re-queue
  [returns work-q seen]
  (go-loop []
    (let [url (<! returns)]
      (assert url)
      (dosync (commute seen conj url))
      (put! work-q url) ; don't respect back-pressure here (need growing buffer *somewhere*)
      (recur))))

(defn wait-for-ack
  [ack seen]
  (let [gate (promise)
        _    (go-loop [i 1]
               (log :waiting-for-ack i)
               (log :acked i (<! ack))
               (if (< i (count @seen))
                 (do (log i :ack/<= (count @seen))
                     (recur (inc i)))
                 (deliver gate :acks-complete!)))]
    gate))

;; Workers ------------------------------------------------------------

(defn proc
  [site-root work-q returns ack seen]
  (async/thread ; will block on I/O
    (loop []
      (let [url  (<!! work-q)
            urls (->> (outgoing-links url site-root)
                      (remove (conj @seen url)))]

        ;; (dosync (commute seen into urls)) ; moved this logic to control
        (log :proc/attempt-to-return url "->" (count urls))
        ;; (async/onto-chan returns urls false) ; TODO: bring me back
        (doseq [u urls] (>!! returns u))
        ;; (prn :proc/returned urls)
        ;; Confirm url has been processed
        (>!! ack url)
        (recur)))))

;; Wiring -------------------------------------------------------------

(defn process-all
  [site-root n]
  (let [seen    (ref #{})
        work-q  (chan 1000 (map (fn [x] (log ::->work-q x) x)))
        returns (chan 10 (map (fn [x] (log ::->returns x) x)))
        ack     (chan 10 (map (fn [x] (log ::->returns x) x)))]
    (dosync (commute seen conj site-root))
    (put! work-q site-root)
    (re-queue returns work-q seen)
    (dotimes [i n]
      (proc site-root work-q returns ack seen))

    ;; (wait-for-ack ack seen)
    @(wait-for-ack ack seen)
    @seen
    #_
    {:work-q  work-q
     :returns returns
     :ack     ack
     :seen    seen}
    #_ (prn @gate)))

;; --------------------------------------------------------------------

(defn print-dir
  [from tos]
  (println from " => ")
  (println ".")
  (doseq [t (butlast tos)]
    (println "├──" t))
  (println "└──" (last tos)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
