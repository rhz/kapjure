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
                         (let [activating-rule-str (print-str (key kv))]
                           (map #(str "  " activating-rule-str " -> " (print-str %) ";")
                                (val kv))))
                       ram)
               (mapcat (fn [kv]
                         (let [inhibiting-rule-str (print-str (key kv))]
                           (map #(str "  " inhibiting-rule-str " -> " (print-str %)
                                      " [color=red,arrowhead=tee];")
                                (val kv))))
                       rim)
               ["}"])
       (write-lines output)))
