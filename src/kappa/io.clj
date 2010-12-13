(ns kappa.io
  {:doc "Functions to read and write Kappa-related data."
   :author "Ricardo Honorato-Zimmer"}
  (:require [kappa.language :as lang]
            [clojure.contrib.duck-streams :as duck-streams]))

(defn- nth-multiple [n coll]
  (map first (partition-all n coll)))

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
       (duck-streams/write-lines output)))

(defn save-table [output result & {:keys [rpd] :or {rpd 1}}]
  (let [obs-exprs (map lang/expr-str (keys (:obs-expr-counts result)))]
    (->> (cons (apply str (interpose \tab (cons "time" obs-exprs)))
               (apply map (fn [& xs]
                            (apply str (interpose \tab xs)))
                      (nth-multiple rpd (:time result))
                      (map #(nth-multiple rpd %)
                           (vals (:obs-expr-counts result)))))
      (duck-streams/write-lines output))))

