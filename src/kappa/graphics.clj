(ns ^{:doc ""
      :author "Ricardo Honorato-Zimmer"}
  kappa.graphics
  (:require [kappa.chamber :as chamber]
            [clojure.contrib.duck-streams :as duck-streams]
            [incanter.core :as incanter]
            [incanter.charts :as charts]))

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

(defn plot-obs-exprs [sim & {title :title :or {:title ""}}]
  (let [time-steps (map :time sim)
        obs-expr-counts (chamber/get-obs-expr-counts sim)
        obs-exprs (keys obs-expr-counts)
        plot (charts/xy-plot time-steps (obs-expr-counts (first obs-exprs))
                             :series-label (print-str (val (ffirst obs-exprs)))
                             :legend true
                             :title title
                             :x-label "Time (s)"
                             :y-label "Number of molecules")]
    (reduce (fn [plot [obs counts]]
              (charts/add-lines plot time-steps counts
                                :series-label (print-str (val (first obs)))))
            plot (rest obs-expr-counts))))

(defn view [chart]
  (incanter/view chart))

