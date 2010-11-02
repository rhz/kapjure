(ns ^{:doc ""
      :author "Ricardo Honorato-Zimmer"}
  kappa.maps
  (:require [kappa.language :as lang]
            [kappa.misc :as misc]
            [clojure.contrib.condition :as c]
            [clojure.contrib.duck-streams :as duck-streams]
            [clojure.set :as set]))

;;; Activation and Inhibition map

(defn- mix [a1 a2 e1-e2]
  (c/handler-case :type
    (let [{name :name, a1states :states, a1bindings :bindings} a1
          {a2states :states, a2bindings :bindings} a2
          sites (set/union (-> a1states keys set) (-> a2states keys set))
          states (into {} (for [site sites]
                            (let [s1 (a1states site), s2 (a2states site)]
                              (cond
                                (or (nil? s1) (= s1 "") (= s1 s2)) [site s2]
                                (or (nil? s2) (= s2 "")) [site s1]
                                :else (c/raise :type :unmixable-agents)))))
          bindings (into {} (for [site sites]
                              (let [b1 (a1bindings site), b2 (a2bindings site)]
                                (cond
                                  (and (= b1 :free) (= b2 :free)) [site :free]
                                  (or (nil? b1) (= b1 :unspecified)) [site b2]
                                  (or (nil? b2) (= b2 :unspecified)) [site b1]
                                  (and (= b1 :semi-link) (number? b2)) [site b2]
                                  (and (= b2 :semi-link) (number? b1)) [site b1]
                                  (and (number? b1) (number? b2)
                                       (= (key (lang/get-lhs-agent b2 e1-e2)) b1)) [site b1]
                                  :else (c/raise :type :unmixable-agents)))))]
      (lang/make-agent name states bindings))
    (handle :unmixable-agents nil)))

(defn- create-mixed-expr [e1 e2]
  (let [[e1-e2 & left] (lang/pair-exprs e1 e2)]
    (loop [e1-e2 e1-e2,
           ids-map (zipmap (apply concat (map keys left)) (repeatedly #(misc/counter))),
           result (into {} (apply concat
                                  (map (partial map (fn [[id a]] [(ids-map id) a])) left)))]
      (if (empty? e1-e2)
        (zipmap (keys result)
                (map #(update-in (val %) [:bindings]
                                 (fn [b] (zipmap (keys b) (replace ids-map (vals b))))) result))

        (let [[oae1 oae2] (first e1-e2)]
          (if-let [mixed-agent (mix (val oae1) (val oae2) e1-e2)]
            (let [id (misc/counter)]
              (recur (rest e1-e2)
                     (into ids-map [{(key oae1) id} {(key oae2) id}])
                     (conj result {id mixed-agent})))
          
            (let [id1 (misc/counter), new-oae1 {id1 (val oae1)},
                  id2 (misc/counter), new-oae2 {id2 (val oae2)}]
              (recur (rest e1-e2)
                     (into ids-map [{(key oae1) id1} {(key oae2) id2}])
                     (into result [new-oae1 new-oae2])))))))))

(defn codomain
  "Returns a map from pairs [oae, site] in p to pairs [oae, site] in e that matches."
  [p e]
  (apply merge
         (let [e-complexes (map lang/subexpr (lang/get-complexes e) (repeat e))]
           (for [p-complex (map lang/subexpr (lang/get-complexes p) (repeat p)),
                 matched-complex (keep (partial lang/match p-complex) e-complexes),
                 [pa-id pa :as p-oae] p-complex,
                 pa-site (-> pa :states keys)]
             (let [[ea-id ea :as e-oae] (first
                                         (lang/subexpr (vector (matched-complex pa-id)) e))]
               {[p-oae pa-site] [e-oae pa-site]})))))

(defn activates? [r1 r2]
  (let [rhs1 (:rhs r1), lhs2 (:lhs r2),
        mss (-> r1 :action meta :modified-sites),
        S (create-mixed-expr rhs1 lhs2),
        [rhs1->S lhs2->S] (map codomain [rhs1 lhs2] (repeat S))
        cod (apply set/intersection (map (comp set vals) [rhs1->S lhs2->S]))
        dom (set (keep identity (for [[pa-site ea-site] rhs1->S]
                                  (when (cod ea-site) pa-site))))]
    (some dom mss)))

(defn activation-map [rules]
  (apply merge
         (for [r1 rules]
           {r1 (filter (partial activates? r1) rules)})))

(defn inhibits? [r1 r2]
  (if (identical? r1 r2)
    false
    (let [lhs1 (:lhs r1), lhs2 (:lhs r2),
          mss (-> r1 :action meta :modified-sites),
          S (create-mixed-expr lhs1 lhs2),
          [lhs1->S lhs2->S] (map codomain [lhs1 lhs2] (repeat S))
          cod (apply set/intersection (map (comp set vals) [lhs1->S lhs2->S]))
          dom (set (keep identity (for [[pa-site ea-site] lhs1->S]
                                    (when (cod ea-site) pa-site))))]
      (some dom mss))))

(defn inhibition-map [rules]
  (apply merge
         (for [r1 rules]
           {r1 (filter (partial inhibits? r1) rules)})))

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


;;; Matching map and Lift map

(defn matching-and-lift-map [rule-set mixture]
  (let [ms (apply map vector ; put what belongs to the matching-map together (same for lift map)
                  (for [r rule-set, cr (lang/get-complexes (:lhs r))] ; for each pair [r, cr]
                    ;; cr, matchings and cm are seqs of ids
                    (let [cr-expr (lang/subexpr cr (:lhs r))
                          ;; filter the complexes in mixture that match cr
                          matchings (keep (fn [cm] (lang/match cr-expr (lang/subexpr cm mixture)))
                                          (lang/get-complexes mixture))]
                      (vector
                       ;; for the matching map
                       {r {cr matchings}}
                       ;; for the lift map
                       (for [cm (map vals matchings), a-id cm, x (-> a-id mixture :states keys)]
                         {a-id {x {:rule r, :complex cr, :embeddings cm}}})))))]
    [(apply merge-with (partial merge-with vector) (first ms))
     (apply merge-with ; if the same agent is found twice merge
            (partial merge-with vector) ; the sites using vector
            (apply concat (second ms)))]))

;;; TODO Observables map: like the matching map

