(ns kappa.sim
  (:require [kappa.chamber :as c]
            [kappa.parser :as p]
            [kappa.graphics :as g]
            :reload)
  (:gen-class))

(defn load-model [filename]
  (p/parse-system (slurp filename)))

(defn -main [filename num-steps]
  (g/view (g/plot-result (c/simulate-and-report (load-model filename)
                                                (Integer/parseInt num-steps)))))

