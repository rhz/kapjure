(ns ^{:doc ""
      :author "Ricardo Honorato-Zimmer"}
  kappa.graphics
  (:require [kappa.chamber :as chamber]
            [clojure.contrib.math :as m]
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

(defn plot-obs-exprs
  "Plot the observed expressions of a seq of chambers."
  [sim & {:keys [title] :or {title ""}}]
  (plot-result (map :time sim) (chamber/get-obs-expr-counts sim) :title title))

(defn- interpolate [t tcps] ; tcps = time-and-counts-pairs
  (if-let [[[t1 v1] [t2 v2]] (first (filter (fn [[[t1 v1] [t2 v2]]]
                                              ;; TODO performance problem here!
                                              (and (>= t t1) (<= t t2))) tcps))]
    (let [slope (/ (- v2 v1) (- t2 t1))]
      (+ v1 (* slope (- t t1))))
    (second (second (last tcps))))) ; = v2

(defn- interpolate-results [sim-results time-steps]
  (let [time-and-counts-pairs (for [{:keys [time obs-expr-counts]} sim-results]
                                (into {} (for [[obs-expr counts] obs-expr-counts]
                                           [obs-expr (partition 2 1 (map vector time counts))])))]
    (for [sim time-and-counts-pairs]
      (into {} (for [[obs-expr tcps] sim]
                 [obs-expr (map #(interpolate % tcps) time-steps)])))))

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
                    (apply concat (map (comp #(map first (partition rpd %)) :time) sim-results)))]
    (get-results sim-results time-steps)))

(defn get-avg-result
  "Interpolate and then average the counts for the observed expressions
  in sim-results for the average of each time step."
  [sim-results & {:keys [rpd] :or {rpd 1}}]
  (let [time-steps (apply map (comp stats/mean vector)
                          (map (comp #(map first (partition rpd %)) :time) sim-results))]
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
  (let [time-steps (apply map (comp stats/mean vector) (map :time sim-results))
        counts (interpolate-results sim-results time-steps)
        counts-mean (apply-to-steps stats/mean counts time-steps)
        counts-error (case error-bars
                       :sd (apply-to-steps stats/sd counts time-steps))]
    ;; TODO add the error bars to this plot
    ;; probably something like (doto (plot-result ...) (add-error-bar ...))
    (plot-result {:time time-steps :obs-expr-counts counts-mean})))

