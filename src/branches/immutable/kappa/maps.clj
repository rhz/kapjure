(ns kappa.maps
  (:use [kappa.language :only (match get-complexes select)]
        [clojure.contrib.duck-streams :only (write-lines)]))

;;; Activation and Inhibition map
;; FIXME activation-map e inhibition-map deben ademas revisar si los agentes que
;; hacen match son modificados por la regla. Ver pag 11 Scalable modeling...
(defn activation-map [rules]
  (zipmap rules
          (map (fn [rhs]
                 (filter #(some identity
                                (for [lhs-complex (get-complexes (:lhs %)),
                                      rhs-complex (get-complexes rhs)]
                                  (match (select rhs-complex rhs)
                                         (select lhs-complex (:lhs %)))))
                         rules))
               (map :rhs rules))))

(defn inhibition-map [rules]
  (->> (map (fn [r]
              (filter #(and (not (identical? r %))
                            (some identity
                                  (let [lhs-1 (:lhs r), lhs-2 (:lhs %)]
                                    (for [lhs-complex-1 (get-complexes lhs-1),
                                          lhs-complex-2 (get-complexes lhs-2)]
                                      (match (select lhs-complex-1 lhs-1)
                                             (select lhs-complex-2 lhs-2))))))
                      rules)) rules)
       (zipmap rules)))

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

;; TODO a test for this function... will be surely a difficult task
(defn matching-and-lift-map [rule-set mixture]
  (let [ms (map vector ; put what belongs to the matching-map together (same for lift map)
                (for [r rule-set, c_r (get-complexes r)] ; for each pair [r, c]
                  ;; c_r, matchings and c_m are seqs of ids
                  ;; filter the complexes in mixture that match c_r
                  (let [matchings (filter #(match (select c_r r) (select % mixture))
                                          (get-complexes mixture))]
                    [{r {c_r matchings}} ; for the matching map
                     (for [c_m matchings, a c_m, x (-> (mixture a) :states keys)] ; a is an id
                       {a {x {:rule r, :complex c_r, :embeddings c_m}}})])))] ; for the lift map
    [(apply merge-with (partial merge-with vector) (first ms))
     (apply merge-with ; if the same agent is found twice merge
            (partial merge-with vector) ; the sites using vector
            (apply concat (second ms)))]))

