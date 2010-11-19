(ns kappa.isomerization
  (:require [kappa.parser :as p]
            [kappa.chamber :as chamber])
  (:gen-class))

(defn -main [n & m]
  (read-line)
  (p/let-rules [r1 "a(x) -> b(x) @ 1"]
    (p/let-exprs [e1 (str (or (first m) 1000) " * a(x)")]
      (let [initial-chamber (time (chamber/make-chamber [r1] e1 1 []))]
        (read-line)
        (println (count (filter (fn [[_ {name :name}]] (= name "a"))
                                (:mixture (time (nth (iterate chamber/gen-event initial-chamber)
                                                     (Integer/parseInt n)))))))))))

