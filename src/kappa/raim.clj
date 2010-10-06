(ns kappa.raim
  (:use [kappa.kappa :only (match)]
        [clojure.contrib.duck-streams :only (write-lines)]))

(defn activation-map [rule-set]
  (zipmap rule-set
          (map (fn [rhs]
                 (filter #(match rhs (:lhs %)) rule-set))
               (map :rhs rule-set))))

(defn inhibition-map [rule-set]
  (zipmap rule-set
          (map (fn [r]
                 (filter #(and (not (= r %))
                               (match (:lhs %) (:lhs %)))
                         rule-set))
               rule-set)))

(defn map2dot [output ram rim]
  (->> (concat ["digraph G {"
                "  node [shape=box];"]
               (map (fn [kv]
                      (let [activating-rule-str (print-str (first kv))]
                        (map #(str "  " activating-rule-str " -> " (print-str %) ";")
                             (second kv))))
                    ram)
               (map (fn [kv]
                      (let [inhibiting-rule-str (print-str (first kv))]
                        (map #(str "  " inhibiting-rule-str " -> " (print-str %)
                                   " [color=red,arrowhead=tee];")
                             (second kv))))
                    rim)
               ["}"])
       (write-lines output)))
