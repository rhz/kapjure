(ns ^{:doc ""
      :author "Ricardo Honorato-Zimmer"}
  kappa.misc)

(defn indexed [s] ; this isn't used anywhere: candidate to be deleted
  (map vector (iterate inc 0) s))

(def third #(nth % 2))

(def xor (comp not =))

(def counter (let [cnt (atom 0)]
               #(swap! cnt inc)))

(defn factorial [n]
  (loop [n n, acc 1]
    (if (= n 1) acc
        (recur (dec n) (* acc n)))))

(defn pre-traverse
  "Traverses a graph depth-first preorder from start, neighbors being a
  function that returns adjacent nodes. Returns a lazy seq of nodes.
  Thanks to jkkramer (Justin Kramer)."
  [neighbors start & {:keys [seen] :or {seen #{}}}]
  (letfn [(step [stack seen]
                (when-let [node (peek stack)]
                  (cons
                   node
                   (lazy-seq
                     (let [nbrs (remove seen (neighbors node))]
                       (step (into (pop stack) nbrs)
                             (into seen nbrs)))))))]
    (step [start]
          (conj seen start))))

(defmulti choice
  "Randomly chooses one key according to the finite probability
  distribution given by the values of m. If m is a vector, the
  distribution is assumed to be uniform."
  class)

(defmethod choice clojure.lang.IPersistentMap [m]
  (let [r (rand (apply + (vals m)))]
    (loop [[[k w] & kws] (seq m)
           sum 0]
      (if (or (nil? kws) (< r (+ sum w))) k
          (recur kws (+ sum w))))))

(defmethod choice clojure.lang.IPersistentVector [v]
  (rand-nth v))

