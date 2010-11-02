(ns ^{:doc ""
      :author "Ricardo Honorato-Zimmer"}
  test.all-tests
  (:use [clojure.contrib.test-is :only [is]])
  (:require [kappa.language :as lang]
            [kappa.maps :as maps]
            [kappa.misc :as misc]
            [kappa.interp :as interp]
            [kappa.parser :as parser]
            [kappa.chamber :as chamber]
            [clojure.contrib.generic.math-functions :as math]
            :reload))


;;;; language.clj
;;; Tests for match :agent
;; match
;; agent equal to pattern
(is (= (lang/match (lang/make-agent :a {:s "phos"} {}) ; a(s~phos)
                   (lang/make-agent :a {:s "phos"} {}))
       true))
(is (= (lang/match (lang/make-agent :a {} {:s :bounded}) ; a(s!1)
                   (lang/make-agent :a {} {:s :bounded}))
       true))
(is (= (lang/match (lang/make-agent :a {:s "phos"} {:s :bounded}) ; a(s~phos!1)
                   (lang/make-agent :a {:s "phos"} {:s :bounded}))
       true))
;; unspecified state in pattern
(is (= (lang/match (lang/make-agent :a {:s ""} {:s :free}) ; a(s)
                   (lang/make-agent :a {:s "phos"} {:s :free})) ; a(s~phos)
       true))
;; unspecified binding in pattern
(is (= (lang/match (lang/make-agent :a {} {:s :unspecified}) ; a(s!?)
                   (lang/make-agent :a {} {:s :bounded})) ; a(s!1)
       true))
;; don't match
;; different name
(is (= (lang/match (lang/make-agent :a {} {}) ; a()
                   (lang/make-agent :b {} {})) ; b()
       false))
;; different state
(is (= (lang/match (lang/make-agent :a {:s "phos"} {}) ; a(s~phos)
                   (lang/make-agent :a {:s "unphos"} {})) ; a(s~unphos)
       false))
;; free site in agent and bound in pattern
(is (= (lang/match (lang/make-agent :a {} {:s :bounded}) ; a(s!1)
                   (lang/make-agent :a {} {:s :free})) ; a(s)
       false))
;; free site in pattern and bound in agent
(is (= (lang/match (lang/make-agent :a {:s "phos"} {:s :free}) ; a(s~phos)
                   (lang/make-agent :a {:s "phos"} {:s :bounded})) ; a(s~phos!1)
       false))

;;; Tests for match :expression
;; match
(interp/def-exprs
  p1 "a(s!1),b(s~phos!1)"
  e1 "a(s~unphos!1),b(s~phos!1)")
(is (lang/expression? p1))
(is (lang/expression? e1))
(is (= (boolean (lang/match p1 e1)) true))
;; don't match... name mismatch
(interp/let-exprs [e2 "b(s~phos!1),b(s!1)"]
  (is (= (boolean (lang/match p1 e2)) false)))

;; match
(interp/let-exprs [p2 "a(s!_)"]
  (is (= (boolean (lang/match p2 e1)) true)))
;; TODO more tests for match :expression


;;;; interp.clj
(is (= (interp/interp (parser/parse-agent "a(x!_)"))
       (lang/make-agent "a" {"x" ""} {"x" :semi-link})))
(is (= (val (first (interp/interp (parser/parse-expr "a(x)"))))
       (lang/make-agent "a" {"x" ""} {"x" :free})))

(interp/let-exprs [e3 "a(x!1), b(x!1)"
                   e4 "a(x~p!1), b(x~u!1)"]
  (is (= (-> e3 first val :name) "a"))
  (is (= (-> e3 first val :states) {"x" ""}))
  (is (= (:name (e4 ((-> e4 first val :bindings) "x"))) "b"))
  (is (= (:states (e4 ((-> e4 first val :bindings) "x"))) {"x" "u"})))


;;;; language.clj
;;; Rules
(interp/let-rules [r1 "a(x), b(x) -> b(x) @ 1"]
  (let [[lhs-rhs created-agents removed-agents] (lang/corr-exprs (:lhs r1) (:rhs r1))
        {eas :elementary-actions} (lang/elementary-actions (:lhs r1) (:rhs r1))]
    (is (= lhs-rhs [[(-> r1 :lhs second) (-> r1 :rhs first)]]))
    (is (= created-agents '()))
    (is (= removed-agents [(-> r1 :lhs first)]))
    (is (= (count eas) 1))
    (interp/let-exprs [e1 "a(x), b(x)", e2 "b(x)"]
      (let [[mm lf] (maps/matching-and-lift-map [r1] e1)
            lhs (:lhs r1)
            a-e1 (-> e1 first key)
            a-r1 (-> lhs first key)
            b-e1 (-> e1 second key)
            b-r1 (-> lhs second key)]
        (is (= mm {r1 {[a-r1] [{a-r1 a-e1}], [b-r1] [{b-r1 b-e1}]}}))
        (is (= lf {a-e1 {"x" {:rule r1, :complex [a-r1], :embeddings [a-e1]}},
                   b-e1 {"x" {:rule r1, :complex [b-r1], :embeddings [b-e1]}}}))
        (let [chamber (chamber/make-chamber [r1] e1 1 0 [])
              chamber-mm (:matching-map chamber)
              new-chamber ((:action r1) chamber (chamber/select-matching (chamber-mm r1)))]
          (is (= mm chamber-mm))
          (is (= (vals (:mixture new-chamber)) (vals e2))))))))


;;;; maps.clj
;;; Activation and Inhibition Maps
(interp/let-rules [r1 "a(x) -> b(x) @ 1"
                   r2 "b(x) -> c(x) @ 1"]
  (is (= (maps/activation-map [r1 r2])
         {r1 [r2], r2 []}))
  (is (= (maps/inhibition-map [r1 r2])
         {r1 [], r2 []})))
;; TODO write more tests for activation-map and inhibition-map

