(ns kappa.tests.language
  (:use [kappa.language :only (match modify-state expression?)]
        [kappa.maps :only (activation-map inhibition-map matching-and-lift-map)]
        [kappa.interp :only (interp def-expressions let-expressions def-rules let-rules)]
        [kappa.misc :only (choice)]
        [kappa.parser :only (parse-agent parse-expression parse-rule)]
        [clojure.contrib.test-is :only (is)]
        :reload-all)
  (:import (kappa.language Agent Rule))
  (:require [clojure.contrib.generic.math-functions :as math]))


;;; Test for modify-state
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


;;; Tests for match :expression
;; match
(def-expressions
  p1 "a(s!1),b(s~phos!1)"
  e1 "a(s~unphos!1),b(s~phos!1)")
(is (expression? p1))
(is (expression? e1))
(is (= (match p1 e1) true))
;; don't match... name mismatch
(let-expressions [e2 "b(s~phos!1),b(s!1)"]
  (is (= (match p1 e2) false)))

;; match
(let-expressions [p2 "a(s!_)"]
  (is (= (match p2 e1) true)))
;; TODO more tests for match :expression


;;; Tests for interp
(is (= (interp (parse-agent "a(x!_)"))
       (Agent. "a" {"x" ""} {"x" :semi-link})))
(is (= (val (first (interp (parse-expression "a(x)"))))
       (Agent. "a" {"x" ""} {"x" :free})))

(let-expressions [e3 "a(x!1), b(x!1)"
                  e4 "a(x~p!1), b(x~u!1)"]
  (is (= (-> e3 first val :name) "a"))
  (is (= (-> e3 first val :states) {"x" ""}))
  (is (= (:name (e4 ((-> e4 first val :bindings) "x"))) "b"))
  (is (= (:states (e4 ((-> e4 first val :bindings) "x"))) {"x" "u"})))


;;; Activation and Inhibition Maps
(let-rules [r1 "a -> b @ 1"
            r2 "b -> c @ 1"]
  (is (= (activation-map [r1 r2])
         {r1 [r2], r2 []}))
  (is (= (inhibition-map [r1 r2])
         {r1 [], r2 []})))
;; TODO write more tests for activation-map and inhibition-map


;; Functions in misc
(is (math/approx= (/ (apply + (for [_ (range 1000)]
                                ;; count how many :b's the next 'for' produces
                                (count (filter #{:b} (for [_ (range 100)]
                                                       (choice {:a 0.3, :b 0.7}))))))
                     (* 1000 100))
                  0.7 ; it should be around this value
                  0.1)) ; epsilon

