(ns crawfish.core
  (:require
   [clojure.core.async :as async :refer [<! <!! >! >!! alts! chan go go-loop]]
   [clojure.inspector :as inspect]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]
   [net.cgrand.enlive-html :as enlive]
   [org.httpkit.client :as kit])
  (:import
   [java.util.concurrent ConcurrentLinkedQueue]
   [javax.swing JFrame])
  (:gen-class))

(set! *warn-on-reflection* true)

;; REPL use only
(comment
  (use 'clojure.repl)
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
                              :value     val}))))

;; # Parsing

;; ## Helpers

(defn page-internal?
  [s]
  (str/starts-with? s "#"))

(defn absolute?
  [s]
  ;; TODO: corner cases?
  ;; TODO: identify urls of the form //cdn.x.y
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

(defn remove-extra-slashes
  [s]
  (str/replace s #"(?<![:/])/+" "/"))

(defn strip-query-params
  [s]
  (str/replace s #"(\?).*" ""))

(defn strip-internal-links
  [s]
  (str/replace s #"#.*" ""))

(defn strip-trailing-slashes
  [s]
  (str/replace s #"/+$" ""))

(defn strip-whitespace
  [s]
  (str/trim s))

(defn interpret-relative-links
  [s]
  (let [interp (str/replace s #"/\w+/../" "/")]
    (if (and (not= interp s) (re-find #"\.\." interp))
      (interpret-relative-links interp)
      interp)))

(defn absolutise [root]
  (fn [url]
    (remove-extra-slashes
     (if (absolute? url)
       url
       (str root "/" url)))))

(def href-sel
  #_ (enlive/attr? :href)
  ;; OR
  [:a (enlive/attr? :href)])

;; ## Main parser

(defn xform-html
  "Takes a byte stream representing an HTML page and a transducer to apply to the sequence of URLs found.
  Returns a set of outgoing links."
  [xform byte-stream]
  (when byte-stream
    (-> byte-stream
        enlive/html-resource
        (enlive/select [href-sel])
        (->> (map #(get-in % [:attrs :href]))
             (into #{} xform)))))

(defn href-transducer
  [site-root]
  (comp (map strip-whitespace)
        (remove page-internal?)
        (remove (external? site-root))
        (remove mailto?)
        (remove tel?)
        (map strip-query-params)
        (map interpret-relative-links)
        (map strip-internal-links)
        (map strip-trailing-slashes)
        (remove empty?)
        (map (absolutise site-root))))

(defn outgoing-links
  "Takes a URL and a site-root, and returns a set of outgoing links."
  [url site-root results]
  (kit/get url
           {:as :stream}
           (fn [res]
             (try (->> res
                       :body
                       (xform-html (href-transducer site-root))
                       (async/onto-chan results))
                  (catch Exception e
                    (log :warn (format "failed to fetch %s" url)))))))

;; # Hierarchical (tree) representation

(def sep #"/")

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
  "Pulls URLs from the return queue and pushes them into the work queue."
  [returns ^ConcurrentLinkedQueue work-q seen]
  (go-loop []
    (let [url (<! returns)]
      (.add work-q url) ; don't respect back-pressure here (need growing buffer *somewhere*)
      (recur))))

(defn wait-for-ack
  "Keeps track of how many URLs have been seen. Blocks until all have been processed,
  or until `timeout` ms have elapsed between acks."
  [ack seen timeout]
  (log :debug :ack/types (type ack) (type seen))
  (let [gate (promise)
        _    (go-loop [i 1]
               (log :debug :waiting-for-ack i)
               (let [[url port] (alts! [ack (async/timeout timeout)])]
                 (cond
                   (= i (count @seen)) (deliver gate {:acks/complete true
                                                      :acks/received i
                                                      :acks/expected (count @seen)})
                   (not= port ack)     (deliver gate {:acks/complete false
                                                      :acks/received i
                                                      :acks/expected (count @seen)})
                   :else               (do (log :info (format "ack (%d / %d) %20s..." i (count @seen) url ))
                                           (recur (inc i))))))]
    gate))

;; # Workers

(defn proc
  "Worker process. Polls from the `work-q`, processes URLs, then pushes outgoing links into `returns`."
  [site-root ^ConcurrentLinkedQueue work-q returns ack seen]
  (async/thread ; will block on I/O
    (loop []
      (when-let [url (.poll work-q)]
        (let [results (chan)]
          (log :debug :proc/got url)
          (outgoing-links url site-root results)
          (go-loop []
            (if-let [out-url (<! results)]
              (do (>! returns out-url) (recur))
              (>! ack url)))))
      (recur))))

;; # Wiring

(defn returns-xform
  "Prevents URLs which have already been seen from being put onto the returns chan
  and registers each new URL as it is encountered."
  [seen]
  (comp (distinct)
        (map (fn [x]
               (dosync (commute seen conj x))
               (log :debug "Added " x " to seen")
               (log :debug :<-----------returns x)
               x))))

(defn process-all
  "Sets up some channels and worker threads, then kicks things off by pushing the site-root
  into the work-queue. Returns a set of all sites discovered."
  [site-root n timeout]
  (let [seen    (ref #{})
        work-q  (ConcurrentLinkedQueue.) ; Want an unbounded queue here to avoid deadlock.
        returns (chan 1 (returns-xform seen))
        ack     (chan 1 (map (fn [x] (log :debug :<-----------ack x) x)))]
    (dotimes [i 10]
      (re-queue returns work-q seen))
    (dotimes [i n]
      (proc site-root work-q returns ack seen))
    (>!! returns site-root)
    (log :info @(wait-for-ack ack seen timeout))
    @seen))

;; # Printing

(defn show-tree!
  "Takes a set of URLs and displays a swing window showing the URLs in a hierarchical view.
  The hierarchy is guessed by splitting each URL into segments and treating the resulting
  sequence as a path from the root."
  [urls quit?]
  (if quit?
    (doto ^JFrame (inspect/inspect-tree (->tree urls))
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
    (inspect/inspect-tree (->tree urls))))

(defn scan
  "Accepts a configuration map as produced by `parse-opts`.
  Depending on `:display` key, either prints site map to STDOUT
  or displays an equivalent Swing window."
  [site-root & [{:keys [display parallelism log-level timeout quit?]
                 :or   {display :edn, parallelism 8, log-level :info timeout 5000 quit? true}}]]
  (binding [*log-level* log-level]
    (let [site-root ((absolutise "https:/") site-root)]
      (log :info :root site-root)
      (case display
        :tree (show-tree! (process-all site-root parallelism timeout) quit?)
        :edn  (do (pprint (->tree (process-all site-root parallelism timeout)))
                  (if quit? (System/exit 0)))))))

;; # CLI options

(def cli-options
  [["-p" "--parallelism P" "Number of I/O threads to use"
    :default 8
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 1 % 64) "Must be a number between 1 and 64"]]
   ["-l" "--log-level L" "Logging level"
    :default :info
    :parse-fn keyword
    :validate [#{:debug :info :warn :error :fatal}]]
   ["-t" "--timeout T" "Max milliseconds between ACKs"
    :default 10000
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 500 % 20000) "Must be a number between 500 and 20000"]]
   ["-d" "--display D" "Display type (tree or EDN)"
    :default :edn
    :parse-fn keyword
    :validate [#{:tree :edn} "Must be one of tree or edn"]]
   ["-h" "--help"]])

(defn pr-exit
  [msg n]
  (println msg)
  (System/exit n))

(defn -main
  "Entrypoint. Parses options, then calls `scan`."
  [& args]
  (let [{:keys [options arguments summary errors] :as parsed}
        (parse-opts args cli-options)]
    (cond
      (or (:help options)
          (empty? arguments))    (pr-exit summary 0)
      errors                     (pr-exit errors 1)
      (not= 1 (count arguments)) (pr-exit "Please supply exactly one site-root" 1)
      :else                      (do (prn :config/ok)
                                     (scan (first arguments) options)))))
