(ns kappa.raim
  (:use [kappa.language :only (match)]
        [clojure.contrib.duck-streams :only (write-lines)]))

(defn activation-map [rule-set]
  (zipmap rule-set
          (map (fn [rhs]
                 (filter #(match rhs (:lhs %)) rule-set))
               (map :rhs rule-set))))

(defn inhibition-map [rule-set]
  (zipmap rule-set
          (map (fn [r]
                 (filter #(and (not (identical? r %))
                               (match (:lhs r) (:lhs %)))
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
