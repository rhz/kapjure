(ns kappa.modelchecking
  {:doc "Functions to convert Kappa models into PRISM and NuSMV models."
   :author "Ricardo Honorato-Zimmer"
  (:require [kappa.language :as lang]))

;; We'll use the rule struct for rule instances
(defn prism-model
  "Convert a kappa model into a PRISM model (for model-checking)"
  [rule-set initial-state & options]
  (let [complexes (lang/reachable-complexes rule-set initial-state)
        reactions (lang/reachable-reactions rule-set initial-state)
        module-names (for [complex complexes]
                       (apply str (interpose "-" (for [a complex] (:name a)))))]
    (doseq [m module-names]
      (println m))))

(defn nusmv-model
  "Convert a kappa model into a boolean NuSMV model (for model-checking)"
  [rule-set initial-state & options]
  (let [reactions (lang/reachable-reactions rule-set initial-state)]
    reactions))

