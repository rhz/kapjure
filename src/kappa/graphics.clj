(ns kappa.graphics
  {:doc "Plotting functions for Kappa simulations."
   :author "Ricardo Honorato-Zimmer"}
  (:require [kappa.chamber :as chamber]
            [clojure.contrib.math :as m]
            [clojure.data.finger-tree :as ft]
            [incanter.core :as incanter]
            [incanter.charts :as charts]
            [incanter.stats :as stats]))

(defn view
  "See incanter.core/view"
  [chart & options]
  (apply incanter/view chart options))

(defn save
  "See incanter.core/save"
  [chart filename & options]
  (apply incanter/save chart filename options))

(defn plot-result
  "Plot the results of a simulation (as returned by kappa.chamber/psimulate)."
  [result & {:keys [title] :or {title ""}}]
  (let [{time-steps :time obs-expr-counts :obs-expr-counts} result
        obs-exprs (keys obs-expr-counts)
        plot (charts/xy-plot time-steps (obs-expr-counts (first obs-exprs))
                             :series-label (print-str (val (ffirst obs-exprs)))
                             :legend true
                             :title title
                             :x-label "Time (s)"
                             :y-label "Number of molecules")]
    (doseq [[obs counts] (rest obs-expr-counts)]
      (charts/add-lines plot time-steps counts
                        :series-label (print-str (val (first obs)))))
    (charts/set-x-range plot 1E-30 (* 1.01 (last time-steps)))
    plot))

(defn- nth-multiple [n coll]
  (map first (partition-all n coll)))

(defn plot-obs-exprs
  "Plot the observed expressions of a seq of chambers."
  [sim & {:keys [title rpd] :or {title "" rpd 1}}]
  (plot-result {:time (nth-multiple rpd (map :time sim))
                :obs-expr-counts (into {} (for [[obs counts] (chamber/get-obs-expr-counts sim)]
                                            [obs (nth-multiple rpd counts)]))}
               :title title))

(let [empty-interval-tree (ft/finger-tree (ft/meter first 0 max))]
  (defn interval-tree [time-steps counts]
    (let [elems (map vector time-steps counts)]
      (into empty-interval-tree elems))))

(defn find-interval [it t]
  (if (empty? it)
    nil
    (let [[l upper-bound _] (ft/split-tree it #(>= % t))
          lower-bound (peek l)]
      [lower-bound upper-bound])))

(defn- interpolate [it t] ;; it = interval tree
  (if-let [[[t1 v1] [t2 v2]] (find-interval it t)]
    (cond
      (nil? t1) v2 ;; t <= first t1
      (> t t2) v2  ;; t >= last t2
      :else (let [slope (/ (- v2 v1) (- t2 t1))]
              (+ v1 (* slope (- t t1)))))
    0)) ;; interval tree is empty

(defn- interpolate-results [sim-results time-steps]
  (for [sim (for [{:keys [time obs-expr-counts]} sim-results]
              (into {} (for [[obs-expr counts] obs-expr-counts]
                         [obs-expr (interval-tree time counts)])))]
    (into {} (for [[obs-expr it] sim]
               [obs-expr (map (partial interpolate it) time-steps)]))))

(defn- apply-to-steps [f counts time-steps]
  (let [obs-exprs (set (for [sim counts
                             [obs-expr _] sim]
                         obs-expr))]
    (into {} (for [obs-expr obs-exprs]
               [obs-expr (apply map (comp f vector) (map #(% obs-expr) counts))]))))

(defn- get-results [sim-results time-steps]
  (let [counts (interpolate-results sim-results time-steps)
        counts-mean (apply-to-steps stats/mean counts time-steps)]
    {:time time-steps :obs-expr-counts counts-mean}))

(defn get-all-results
  "Interpolate and then average the counts for the observed expressions
  in sim-results for all time steps."
  [sim-results & {:keys [rpd] :or {rpd 1}}]
  (let [time-steps (distinct
                    (apply concat (map (comp (partial nth-multiple rpd) :time) sim-results)))]
    (get-results sim-results time-steps)))

(defn- mean [& xs]
  (/ (apply + xs) (count xs)))

(defn get-avg-result
  "Interpolate and then average the counts for the observed expressions
  in sim-results for the average of each time step."
  [sim-results & {:keys [rpd] :or {rpd 1}}]
  (let [time-steps (apply map mean (map (comp (partial nth-multiple rpd) :time) sim-results))]
    (get-results sim-results time-steps)))

(defn get-most-repr-result
  "Calls kappa.graphics/get-all-results and then return the simulation
  in sim-results that is most similar to the interpolated result."
  [sim-results & {:keys [norm rpd] :or {norm :supremum rpd 1}}]
  (let [results (get-all-results sim-results :rpd rpd)
        ;; FIXME (map vector results) don't make sense
        ;; as results is a map {:time ... :obs-expr-counts ...}
        results-map (into {} (map vector results))
        norm (fn [x]
               (case norm
                 :supremum (fn [y] (m/expt (- x y) 2)))) ; FIXME is this the supremum norm?
        error (map #(map (norm (results-map %)) %)
                   ;; FIXME next form returns a seq of maps, not seqs
                   (map :obs-expr-counts sim-results))
        error-to-sim (zipmap (map (partial apply +) error) sim-results)]
    (error-to-sim (apply min (keys error-to-sim)))))

(defn mean-plot
  "Plot the average behaviour of the simulations in sim-results."
  [sim-results & {:keys [error-bars] :or {error-bars :sd}}]
  (let [time-steps (apply map mean (map :time sim-results))
        counts (interpolate-results sim-results time-steps)
        counts-mean (apply-to-steps stats/mean counts time-steps)
        counts-error (case error-bars
                       :sd (apply-to-steps stats/sd counts time-steps))]
    ;; TODO add the error bars to this plot
    ;; probably something like (doto (plot-result ...) (add-error-bar ...))
    (plot-result {:time time-steps :obs-expr-counts counts-mean})))

