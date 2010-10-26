(ns kappa.misc)

(defn indexed [s] ; this isn't used anywhere: candidate to be deleted
  (map vector (iterate inc 0) s))

(def third #(nth % 2))

(def counter (let [cnt (atom 0)]
               #(swap! cnt inc)))

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

(defn choice
  "Randomly chooses one key according to the finite probability
  distribution given by the values of m. If m is a vector, the
  distribution is assumed to be uniform."
  [m]
  (let [m (if (map? m) m
              (zipmap m (repeat 1)))]
    (let [r (rand (apply + (vals m)))]
      (loop [[[k w] & kws] (seq m)
             sum 0]
        (if (or (nil? kws) (< r (+ sum w))) k
            (recur kws (+ sum w)))))))

