(ns kappa.tests.language
  (:use kappa.language
        [clojure.contrib.test-is :only (is)]))

;;; Tests for kype
(def a1 (struct k-agent :a {} {}))
(is (= (kype a1) :agent))
(is (= (kype []) :complex))
(is (= (kype [a1]) :complex))
(is (= (kype [[a1]]) :expression))
(is (= (kype 1) nil))

;; Test for modify-state
(is (= (:states (modify-state (struct k-agent :a {:s "phos"} {})
                              :s "unphos"))
       {:s "unphos"}))

;;; Tests for match :agent
;; match
;; agent equal to pattern
(is (= (match (struct k-agent :a {:s "phos"} {}) ; a(s~phos)
              (struct k-agent :a {:s "phos"} {}))
       true))
(is (= (match (struct k-agent :a {} {:s :bounded}) ; a(s!1)
              (struct k-agent :a {} {:s :bounded}))
       true))
(is (= (match (struct k-agent :a {:s "phos"} {:s :bounded}) ; a(s~phos!1)
              (struct k-agent :a {:s "phos"} {:s :bounded}))
       true))
;; unspecified state in pattern
(is (= (match (struct k-agent :a {:s ""} {:s :free}) ; a(s)
              (struct k-agent :a {:s "phos"} {:s :free})) ; a(s~phos)
       true))
;; unspecified binding in pattern
(is (= (match (struct k-agent :a {} {:s :unspecified}) ; a(s!?)
              (struct k-agent :a {} {:s :bounded})) ; a(s!1)
       true))
;; don't match
;; different name
(is (= (match (struct k-agent :a {} {}) ; a()
              (struct k-agent :b {} {})) ; b()
       false))
;; different state
(is (= (match (struct k-agent :a {:s "phos"} {}) ; a(s~phos)
              (struct k-agent :a {:s "unphos"} {})) ; a(s~unphos)
       false))
;; free site in agent and bound in pattern
(is (= (match (struct k-agent :a {} {:s :bounded}) ; a(s!1)
              (struct k-agent :a {} {:s :free})) ; a(s)
       false))
;; free site in pattern and bound in agent
(is (= (match (struct k-agent :a {:s "phos"} {:s :free}) ; a(s~phos)
              (struct k-agent :a {:s "phos"} {:s :bounded})) ; a(s~phos!1)
       false))

;;; Tests for match :complex
;; match
(def-agents
  pa1 {:name :a, :states {:s ""}, :bindings {:s pa2}} ; extended form
  pa2 [:b {:s "phos"} {:s pa1}] ; compact form :)
  a1 [:a {:s "unphos"} {:s a2}]
  a2 [:b {:s "phos"} {:s a1}])
(is (= (match [@pa1 @pa2] [@a2 @a1]) true))
;; don't match... name mismatch
(def-agents a3 [:b {:s ""} {:s a2}])
(is (= (match [@pa1 @pa2] [@a3 @a2]) false))

;; match
(let-agents [pa3 [:a {:s ""} {:s :semi-link}]]
  (is (= (match [@pa3] [@a1 @a2]) true)))
;; TODO more tests for match :complex

