(ns crawfish.core-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [crawfish.core :refer :all]
   [net.cgrand.enlive-html :as enlive]))

(deftest strip-query-params-test
  (is (= (strip-query-params "foo.bar.com/yeah?foo=2&y=not")
         "foo.bar.com/yeah")))

(deftest remove-extra-slashes-test
  (is (= "https://clojars.org/cljsjs/react-ultimate-pagination"
         (remove-extra-slashes
          "https://clojars.org//cljsjs/react-ultimate-pagination"))))

(deftest absolutise-test
  (let [root "daveduthie.github.io"
        datum "http://daveduthie.github.io"]
    ;; TODO: a better regex would allow a more natural `(absolutise "http://")`
    (is (= datum ((absolutise "http:/") root)))))

(deftest interpret-relative-links-test
  (testing "valid relative links"
    (let [url   "http://daveduthie.github.io/foo/doo/../../bar/../yo"
          datum "http://daveduthie.github.io/yo"]
      (is (= datum (interpret-relative-links url)))))
  (testing "invalid (?) relative links"
    (let [url   "http://daveduthie.github.io/../yo"
          datum "http://daveduthie.github.io/../yo"]
      (is (= datum (interpret-relative-links url))))))

(deftest transducer-test
  (let [site-root "http://foo.bar"
        xform     (comp (remove page-internal?)
                        (remove absolute?)
                        (remove mailto?)
                        (remove tel?)
                        (map strip-query-params)
                        (map strip-internal-links)
                        (map strip-trailing-slashes)
                        (map interpret-relative-links)
                        (map (absolutise site-root)))
        urls      ["http://remove.me.bar"
                   "/a"
                   "/b/"
                   "/c#d"
                   "e/f/../g"]
        datum     ["http://foo.bar/a"
                   "http://foo.bar/b"
                   "http://foo.bar/c"
                   "http://foo.bar/e/g"]]
    (is (= datum (into [] xform urls)))))

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

(deftest ->tree-test
  (let [links #{"https://foo.github.io/static/images/favicon.png"
                "https://foo.github.io/static/images/foo.png"
                "https://foo.github.io/articles/yo-ho-ho"}
        datum {"foo.github.io" {"articles" {"yo-ho-ho" {}},
                                "static"   {"images" {"favicon.png" {},
                                                      "foo.png"     {}}}}}]
    (is (= datum 
           (->tree links)))))
