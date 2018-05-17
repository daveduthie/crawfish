(ns crawfish.core-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [crawfish.core :refer :all]
   [net.cgrand.enlive-html :as enlive]))

(deftest strip-query-params-test
  (is (= (strip-query-params "foo.bar.com/yeah?foo=2&y=not")
         "foo.bar.com/yeah")))

;; (enlive/html-resource)
(deftest xform-html-test
  (let [site-root "https://daveduthie.github.io"
        stream    (io/input-stream (io/resource "David Duthie.html"))
        datum     #{"https://daveduthie.github.io/2018-01-12.html"
                    "https://daveduthie.github.io/2018-05-03.html"
                    "https://daveduthie.github.io/2018-02-12_2.html"
                    "https://daveduthie.github.io/2018-03-15.html"
                    "https://daveduthie.github.io/2018-02-13.html"
                    "https://daveduthie.github.io/2018-01-10.html"
                    "https://daveduthie.github.io/2018-02-12.html"
                    "https://daveduthie.github.io/2018-04-11.html"
                    "https://daveduthie.github.io/2018-03-08.html"
                    "https://daveduthie.github.io/resume.pdf"
                    "https://daveduthie.github.io/resume.html"}]
    (is (= datum
           (xform-html stream
                       (comp (remove (external? site-root))
                             (remove mailto?)
                             (remove tel?)
                             (remove page-internal?)
                             (map strip-query-params)
                             (map (absolutise site-root))))))))

(deftest remove-extra-slashes-test
  (is (= "https://clojars.org/cljsjs/react-ultimate-pagination"
         (remove-extra-slashes
          "https://clojars.org//cljsjs/react-ultimate-pagination"))))
