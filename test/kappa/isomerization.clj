(ns kappa.isomerization
  (:require [kappa.interp :as interp]
            [kappa.chamber :as chamber])
  (:gen-class))

(defn -main [n & m]
  (read-line)
  (interp/let-rules [r1 "a(x) -> b(x) @ 1"]
    (interp/let-exprs [e1 (str (or (first m) 1000) " * a(x)")]
      (let [initial-chamber (time (chamber/make-chamber [r1] e1 1 0 []))]
        (read-line)
        (println (count (filter (fn [[_ {name :name}]] (= name "a"))
                                (:mixture (time (nth (iterate chamber/gen-event initial-chamber)
                                                     (Integer/parseInt n)))))))))))

