(ns kappa.maps
  (:use [kappa.language :only (match)]
        [clojure.contrib.duck-streams :only (write-lines)]))

;;; Activation and Inhibition map
;; FIXME activation-map e inhibition-map deben ademas revisar si los agentes que
;; hacen match son modificados por la regla. Ver pag 11 Scalable modeling...
(defn activation-map [rule-set]
  (zipmap rule-set
          (map (fn [rhs]
                 (filter #(some identity
                                (for [lhs-complex (:lhs %), rhs-complex rhs]
                                  (match rhs-complex lhs-complex)))
                         rule-set))
               (map :rhs rule-set))))

(defn inhibition-map [rule-set]
  (zipmap rule-set
          (map (fn [r]
                 (filter #(and (not (identical? r %))
                               (some identity
                                     (for [lhs-complex-1 (:lhs r),
                                           lhs-complex-2 (:lhs %)]
                                       (match lhs-complex-1 lhs-complex-2))))
                         rule-set))
               rule-set)))

(defn map2dot [output ram rim]
  (->> (concat ["digraph G {"
                "  node [shape=box];"]
               (mapcat (fn [kv]
                         (let [activating-rule-name (:name (key kv))]
                           (map #(str "  " activating-rule-name " -> " (:name %) ";")
                                (val kv))))
                       ram)
               (mapcat (fn [kv]
                         (let [inhibiting-rule-name (:name (key kv))]
                           (map #(str "  " inhibiting-rule-name " -> " (:name %)
                                      " [color=red,arrowhead=tee];")
                                (val kv))))
                       rim)
               ["}"])
       (write-lines output)))


;;; Matching map and Lift map

;; TODO a test for this function... will be a daunting task
(defn matching-and-lift-map [rule-set mixture]
  (-> (for [r rule-set, c_r r]
        (let [matchings (filter (partial match c_r) mixture)]
          [{r {c_r matchings}} ; for the matching map
           ;; for the lift map
           (-> (for [c_m matchings, a c_m, x (-> a :states keys)]
                 {a {x {:rule r, :complex c_r, :embeddings c_m}}})
               (partial apply merge-with merge))]))
      (map (partial apply merge-with merge))))

