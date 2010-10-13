(ns kappa.tests.language
  (:use kappa.language
        kappa.raim
        [kappa.parser :only (parse-agent parse-expression parse-rule)]
        [clojure.contrib.test-is :only (is)])
  (:import (kappa.language Agent Rule)))

;; Test for modify-state
(is (= (:states (modify-state (Agent. :a {:s "phos"} {})
                              :s "unphos"))
       {:s "unphos"}))

;;; Tests for match :agent
;; match
;; agent equal to pattern
(is (= (match (Agent. :a {:s "phos"} {}) ; a(s~phos)
              (Agent. :a {:s "phos"} {}))
       true))
(is (= (match (Agent. :a {} {:s :bounded}) ; a(s!1)
              (Agent. :a {} {:s :bounded}))
       true))
(is (= (match (Agent. :a {:s "phos"} {:s :bounded}) ; a(s~phos!1)
              (Agent. :a {:s "phos"} {:s :bounded}))
       true))
;; unspecified state in pattern
(is (= (match (Agent. :a {:s ""} {:s :free}) ; a(s)
              (Agent. :a {:s "phos"} {:s :free})) ; a(s~phos)
       true))
;; unspecified binding in pattern
(is (= (match (Agent. :a {} {:s :unspecified}) ; a(s!?)
              (Agent. :a {} {:s :bounded})) ; a(s!1)
       true))
;; don't match
;; different name
(is (= (match (Agent. :a {} {}) ; a()
              (Agent. :b {} {})) ; b()
       false))
;; different state
(is (= (match (Agent. :a {:s "phos"} {}) ; a(s~phos)
              (Agent. :a {:s "unphos"} {})) ; a(s~unphos)
       false))
;; free site in agent and bound in pattern
(is (= (match (Agent. :a {} {:s :bounded}) ; a(s!1)
              (Agent. :a {} {:s :free})) ; a(s)
       false))
;; free site in pattern and bound in agent
(is (= (match (Agent. :a {:s "phos"} {:s :free}) ; a(s~phos)
              (Agent. :a {:s "phos"} {:s :bounded})) ; a(s~phos!1)
       false))

;;; Tests for match :complex
;; match
(def-agents
  pa1 {:name :a, :states {:s ""}, :bindings {:s pa2}} ; extended form
  pa2 [:b {:s "phos"} {:s pa1}] ; compact form :)
  a1 [:a {:s "unphos"} {:s a2}]
  a2 [:b {:s "phos"} {:s a1}])
(is (= (match [pa1 pa2] [a2 a1]) true))
;; don't match... name mismatch
(def-agents a3 [:b {:s ""} {:s a2}])
(is (= (match [pa1 pa2] [a3 a2]) false))

;; match
(let-agents [pa3 [:a {:s ""} {:s :semi-link}]]
  (is (= (match [pa3] [a1 a2]) true)))
;; TODO more tests for match :complex

;; Tests for interp
(is (= (interp (parse-agent "a(x!_)"))
       (Agent. "a" {"x" ""} {"x" :semi-link})))
(is (= @(first (interp (parse-expression "a(x)")))
       (Agent. "a" {"x" ""} {"x" :free})))
(is (= (:states @(first (interp (parse-expression "a(x!1), b(x!1)"))))
       {"x" ""}))
;; FIXME why first returns b(x~u!1)?
(is (= (:name @(first (interp (parse-expression "a(x!1), b(x!1)"))))
       "b"))
(is (= (:states @((:bindings @(first (interp (parse-expression "a(x~p!1), b(x~u!1)")))) "x"))
       {"x" "p"}))
(is (= (:name @((:bindings @(first (interp (parse-expression "a(x~p!1), b(x~u!1)")))) "x"))
       "a"))

;; Activation and Inhibition Maps
(let-rules [r1 "a -> b @ 1"
            r2 "b -> c @ 1"]
  (is (= (activation-map [r1 r2])
         {r1 [r2], r2 []}))
  (is (= (inhibition-map [r1 r2])
         {r1 [], r2 []})))
;; TODO write more tests for activation-map and inhibition-map

