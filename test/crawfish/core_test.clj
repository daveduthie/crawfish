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
        xform     (href-transducer site-root) #_ (comp (remove page-internal?)
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

(deftest transducer-test-2
  (let [site-root "https://functionaljobs.com/"
        xform     (href-transducer site-root)
        links     (map
                   #(get-in % [:attrs :href]) 
                   '({:tag   :a,
                      :attrs {:class "logo", :href "https://functionaljobs.com/"},
                      :content
                      ({:tag     :img,
                        :attrs
                        {:src
                         "https://functionaljobs.com/static/images/logo.png?1445284800",
                         :alt "Functional Jobs"},
                        :content nil})}
                     {:tag     :a,
                      :attrs   {:href "https://functionaljobs.com/", :class "current"},
                      :content ("Browse Jobs")}
                     {:tag     :a,
                      :attrs   {:href "https://functionaljobs.com/jobs/add/"},
                      :content ("Post a Job")}
                     {:tag     :a,
                      :attrs   {:href "https://functionaljobs.com/candidates/new/"},
                      :content ("My Profile")}
                     {:tag     :a,
                      :attrs   {:href "https://functionaljobs.com/users/login/"},
                      :content ("Log In")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9086-software-developer-flowerpilot-m-f-at-lambdawerk-gmbh"},
                      :content ("Software Developer (Flowerpilot) (m/f)")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9085-senior-full-stack-software-engineer-at-circleci"},
                      :content ("Senior Full Stack Software Engineer")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9084-principal-software-engineer-at-circleci"},
                      :content ("Principal Software Engineer")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9083-damn-good-developer-at-reaktor"},
                      :content ("Damn Good Developer")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9081-backend-developer-at-gomore"},
                      :content ("Backend developer")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9080-software-engineer-developer-at-itprotv"},
                      :content ("Software Engineer / Developer")}
                     {:tag :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9079-ocaml-engineer-for-pl-distributed-systems-cryptocurrency-project-at-o1-labs"},
                      :content
                      ("OCaml Engineer for PL/distributed-systems cryptocurrency project")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9078-frontend-engineer-at-trivium-real-estate"},
                      :content ("Frontend Engineer")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9076-clojure-engineer-at-red-pineapple-media"},
                      :content ("Clojure Engineer")}
                     {:tag :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9075-software-engineer-haskell-full-stack-singapore-on-site-at-capital-match"},
                      :content
                      ("Software Engineer (Haskell - Full Stack - Singapore - On Site)")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://functionaljobs.com/jobs/9026-ocaml-server-side-developer-at-ahrefs"},
                      :content ("OCaml server-side developer")}
                     {:tag     :a,
                      :attrs
                      {:class "rss", :href "https://functionaljobs.com/jobs/?format=rss"},
                      :content ("RSS")}
                     {:tag     :a,
                      :attrs
                      {:class "email",
                       :href  "https://functionaljobs.com/subscriptions/add/"},
                      :content ("Email Alerts")}
                     {:tag :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/b1e139a6-59f8-11e8-9f6d-1ef185e4d706",
                       :target "_blank"},
                      :content
                      ("Senior DevOps Automation Engineer, Apple Retail Engineering")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/0868aae2-5937-11e8-99b6-7409c39a5e30",
                       :target "_blank"},
                      :content ("Siri - Knowledge Software Engineer")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/afa095b2-5866-11e8-91de-b498e9acad1d",
                       :target "_blank"},
                      :content ("Data Scientist")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/db3a8d90-5848-11e8-831a-b15cd118be27",
                       :target "_blank"},
                      :content ("SPELANDE UTVECKLARE TILL IGDB.COM")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/8826545a-583d-11e8-88b1-15da5fba44c9",
                       :target "_blank"},
                      :content ("SPELANDE OCH UTVECKLANDE CTO SÖKES TILL IGDB.COM!")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/3f92874a-57b1-11e8-96c3-631e7596508e",
                       :target "_blank"},
                      :content ("Sr. Software Engineer, iTunes Editorial Engineering")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/43e417de-574b-11e8-9bf7-cbc86c208332",
                       :target "_blank"},
                      :content ("Software Engineer (m/f)")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/4e35eacc-5689-11e8-9bdc-d787db7fbdca",
                       :target "_blank"},
                      :content ("Web Developer")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://weworkremotely.com/remote-jobs/circleci-back-end-engineering-manager",
                       :target "_blank"},
                      :content ("Back End Engineering Manager")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/b081d4ce-5554-11e8-9483-2a957b764971",
                       :target "_blank"},
                      :content ("Senior Software Developer")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://weworkremotely.com/remote-jobs/virtualq-gmbh-senior-software-engineer-ruby-python-or-haskell",
                       :target "_blank"},
                      :content ("Senior Software Engineer - Ruby, Python or Haskell")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/82cfa880-5356-11e8-9951-36975606a9ae",
                       :target "_blank"},
                      :content ("Young Professional | Puzzelaar")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/c704a9ce-52d9-11e8-8926-4830e746a6bd",
                       :target "_blank"},
                      :content ("Senior Backend Engineer")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "http://jobs.github.com/positions/aac74052-52be-11e8-95ba-fc0c6390a60f",
                       :target "_blank"},
                      :content ("Backend Engineer - Elixir")}
                     {:tag     :a,
                      :attrs
                      {:href
                       "https://weworkremotely.com/remote-jobs/reify-health-front-end-software-engineer",
                       :target "_blank"},
                      :content ("Front End Software Engineer")}
                     {:tag     :a,
                      :attrs   {:href "https://functionaljobs.com/jobs/add/"},
                      :content ("Post a Job")}
                     {:tag     :a,
                      :attrs   {:href "https://functionaljobs.com/testimonials/"},
                      :content ("See what others have to say →")}
                     {:tag     :a,
                      :attrs   {:href "https://functionaljobs.com/terms-of-service/"},
                      :content ("Terms")}
                     {:tag     :a,
                      :attrs   {:href "https://functionaljobs.com/privacy-policy/"},
                      :content ("Privacy")}
                     {:tag     :a,
                      :attrs   {:href "https://functionaljobs.com/testimonials/"},
                      :content ("Testimonials")}
                     {:tag     :a,
                      :attrs   {:href "https://functionaljobs.com/contact/"},
                      :content ("Contact Us")}
                     {:tag     :a,
                      :attrs
                      {:href            "https://twitter.com/functionaljobs",
                       :class           "socialite twitter-follow",
                       :data-show-count "true"},
                      :content ("Follow @functionaljobs")}))
        datum     ["https://functionaljobs.com"
                   "https://functionaljobs.com"
                   "https://functionaljobs.com/jobs/add"
                   "https://functionaljobs.com/candidates/new"
                   "https://functionaljobs.com/users/login"
                   "https://functionaljobs.com/jobs/9086-software-developer-flowerpilot-m-f-at-lambdawerk-gmbh"
                   "https://functionaljobs.com/jobs/9085-senior-full-stack-software-engineer-at-circleci"
                   "https://functionaljobs.com/jobs/9084-principal-software-engineer-at-circleci"
                   "https://functionaljobs.com/jobs/9083-damn-good-developer-at-reaktor"
                   "https://functionaljobs.com/jobs/9081-backend-developer-at-gomore"
                   "https://functionaljobs.com/jobs/9080-software-engineer-developer-at-itprotv"
                   "https://functionaljobs.com/jobs/9079-ocaml-engineer-for-pl-distributed-systems-cryptocurrency-project-at-o1-labs"
                   "https://functionaljobs.com/jobs/9078-frontend-engineer-at-trivium-real-estate"
                   "https://functionaljobs.com/jobs/9076-clojure-engineer-at-red-pineapple-media"
                   "https://functionaljobs.com/jobs/9075-software-engineer-haskell-full-stack-singapore-on-site-at-capital-match"
                   "https://functionaljobs.com/jobs/9026-ocaml-server-side-developer-at-ahrefs"
                   "https://functionaljobs.com/jobs"
                   "https://functionaljobs.com/subscriptions/add"
                   "https://functionaljobs.com/jobs/add"
                   "https://functionaljobs.com/testimonials"
                   "https://functionaljobs.com/terms-of-service"
                   "https://functionaljobs.com/privacy-policy"
                   "https://functionaljobs.com/testimonials"
                   "https://functionaljobs.com/contact"]]
    (is (= datum (into [] xform links)))))

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

(comment
  (require '[clojure.test :refer [run-tests]])

  (run-tests 'crawfish.core-test)

  )
