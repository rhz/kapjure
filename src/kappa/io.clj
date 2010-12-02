(ns ^{:doc ""
      :author "Ricardo Honorato-Zimmer"}
  kappa.io
  (:require [clojure.contrib.duck-streams :as duck-streams]))

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

