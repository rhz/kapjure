(ns kappa.maps
  {:doc "Fns to create the activation map, inhibition map, matching map and lift map."
   :author "Ricardo Honorato-Zimmer"}
  (:require [kappa.language :as lang]
            [kappa.misc :as misc]
            [clojure.set :as set]
            [clojure.contrib.condition :as c]))

;;; Activation and Inhibition map

(defn mix
  "Tries to mix a1 with a2 in one single agent. If it can't returns nil."
  [a1 a2 e1-e2]
  (c/handler-case :type
    (let [{name :name, a1states :states, a1bindings :bindings} a1
          {a2states :states, a2bindings :bindings} a2
          sites (set/union (-> a1states keys set) (-> a2states keys set))
          states (into {} (for [site sites
                                :let [s1 (a1states site), s2 (a2states site)]]
                            (cond
                              (or (nil? s1) (= s1 "") (= s1 s2)) [site s2]
                              (or (nil? s2) (= s2 "")) [site s1]
                              :else (c/raise :type :unmixable-agents))))
          bindings (into {} (for [site sites
                                  :let [b1 (a1bindings site), b2 (a2bindings site)]]
                              (cond
                                (and (= b1 :free) (= b2 :free)) [site :free]
                                (or (nil? b1) (= b1 :unspecified)) [site b2]
                                (or (nil? b2) (= b2 :unspecified)) [site b1]
                                (and (= b1 :semi-link) (number? b2)) [site b2]
                                (and (= b2 :semi-link) (number? b1)) [site b1]
                                (and (number? b1) (number? b2)) [site b1]
                                :else (c/raise :type :unmixable-agents))))]
      (lang/make-agent name states bindings))
    (handle :unmixable-agents nil)))

(defn cod-intersection-in-dom [e1 e2]
  (let [[e1-e2] (lang/pair-exprs e1 e2)]
    (for [[[id1 a1] [id2 a2]] e1-e2
          :let [amixed (mix a1 a2 e1-e2)]
          :when amixed
          site (keys (:states amixed))]
      [id1 site id2])))

(defn modified-site-in-intersection [mss e1 e2]
  (let [is (cod-intersection-in-dom e1 e2)
        mss-in-intersection (filter (comp (set mss) (partial take 2)) is)
        complex-expr (fn [e id] (lang/subexpr e (lang/complex e id)))]
    (if (seq mss-in-intersection)
      (some (fn [[id1 _ id2]]
              (or (lang/match-expr (complex-expr e1 id1) (complex-expr e2 id2))
                  (lang/match-expr (complex-expr e2 id2) (complex-expr e1 id1))))
            mss-in-intersection)
      nil)))

(defn activates?
  "Tells whether r1 activates r2."
  [r1 r2]
  (boolean (modified-site-in-intersection (-> r1 :action meta :modified-sites)
                                          (:rhs r1) (:lhs r2))))

(defn activation-map
  "Returns a map from each rule r in rules to the rules r activates."
  [rules]
  (apply merge
         (for [r1 rules]
           {r1 (filter (partial activates? r1) rules)})))

(defn inhibits?
  "Tells whether r1 inhibits r2."
  [r1 r2]
  (and (not (identical? r1 r2))
       (boolean (modified-site-in-intersection (-> r1 :action meta :modified-sites)
                                               (:lhs r1) (:lhs r2)))))

(defn inhibition-map
  "Returns a map from each rule r in rules to the rules r inhibits."
  [rules]
  (apply merge
         (for [r1 rules]
           {r1 (filter (partial inhibits? r1) rules)})))


;;; Matching map and Lift map
(comment
(defn map-compare [m1 m2]
  (compare (vec (sort-by > (vals m1))) (vec (sort-by > (vals m2)))))

  (let [h1 (hash m1)
        h2 (hash m2)]
    (cond
      (< h1 h2) -1
      (> h1 h2) 1
      (and (= h1 h2) (= m1 m2)) 0
      ;; hash collision
      :else (let [c1 (count m1)
                  c2 (count m2)]
              (cond
                (< c1 c2) -1
                (= c1 c2) (compare (vec (sort-by > (vals m1))) (vec (sort-by > (vals m2))))
                (> c1 c2) 1)))))

(defrecord LiftMapValue [rule complex emb])

(defn make-lm-val [rule complex emb]
  (LiftMapValue. rule complex emb))

(defn matching-and-lift-map
  "Compute the matching map and lift map. For reference, see
  'Scalable Simulation of Cellular Signaling Networks', V. Danos, J. Feret,
  W. Fontana, and J. Krivine, 2007."
  [rule-set mixture]
  (->> (for [r rule-set
             cr (-> r :lhs meta :complexes)
             :let [cr-expr (lang/subexpr (:lhs r) cr)

                   embs (remove empty?
                                (for [cm-expr (lang/complexes mixture)]
                                  (lang/domain2codomain mixture [cr-expr] [cm-expr])))

                   ;; for each cr make a map from rule agent ids to mixture agent ids
                   matchings (for [emb embs]
                               (into {} (for [[[ar-id _] [am-id  _]] emb]
                                          [ar-id am-id])))]]

         [{r {cr (set matchings)}} ;; => matching-map
          ;;{r {cr (apply ft/counted-sorted-set-by map-compare matchings)}}
          (for [emb embs
                [_ [cod-id cod-site]] emb]
            {cod-id {cod-site #{(make-lm-val r cr emb)}}})]) ;; => lift-map

    (apply map vector) ; put what belongs to the matching-map together in a vector ...
    ;; ... and what belongs to the lift-map together in another vector
    ((partial map #(%1 %2)
              [(partial apply merge-with merge) ;; if the same rule is found twice in the
               ;; matching map, merge the maps {cr matchings}, as cr won't appear twice
               (comp (partial apply merge-with ;; if the same agent is found twice in the
                              ;; lift map, merge the maps associated with its sites using
                              ;; vector and then set (in lift map, sites may appear twice)
                              (partial merge-with into))
                     ;; but first make just one big list of all the maps
                     (partial apply concat))]))))

