(ns kappa.modelchecking
  (:use kappa.parser))

;; We'll use the rule struct for rule instances
(defn prism-model
  "Convert a kappa model into a PRISM model (for model-checking)"
  [rule-set initial-state & options]
  (let [complexes (reachable-complexes rule-set initial-state)
        reactions (reachable-reactions rule-set initial-state)
        module-names (for [complex complexes]
                       (apply str (interpose "-" (for [a complex] (:name a)))))]
    (doseq [m module-names]
      (println m))
    nil))

(defn nusmv-model
  "Convert a kappa model into a boolean NuSMV model (for model-checking)"
  [rule-set initial-state & options]
  (let [reactions (reachable-reactions rule-set initial-state)]
    nil))
