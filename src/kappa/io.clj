(ns kappa.io
  {:doc "Functions to read and write Kappa-related data."
   :author "Ricardo Honorato-Zimmer"}
  (:require [kappa.language :as lang]
            [kappa.chamber :as c]
            [clojure.contrib.io :as io]
            [clojure.contrib.duck-streams :as duck-streams])
  (:import java.util.Date))

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

(defn simulate-write-and-report
  [output chamber num-steps & callbacks]
  (with-open [outfile (io/writer output)]
    (let [obs-exprs (keys (c/get-obs-expr-counts chamber))
          n (quot num-steps 10)]
      (.write outfile (apply str (interpose \tab (cons "time"
                                                       (map lang/expr-str obs-exprs)))))
      (loop [c chamber
             i 1]
        (.write outfile "\n")
        (.write outfile (apply str
                               (interpose \tab (cons (:time c)
                                                     (map (c/get-obs-expr-counts c) obs-exprs)))))
        (when (zero? (rem i n))
          (println "Done" (str (* 100.0 (/ i num-steps)) "% -")
                   (.toString (Date.))))
        (if (< i num-steps)
          (recur (c/gen-event c) (inc i))
          (println "Finished"))))))

