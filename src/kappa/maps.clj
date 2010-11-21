(ns ^{:doc "Fns to create the activation map, inhibition map, matching map and lift map."
      :author "Ricardo Honorato-Zimmer"}
  kappa.maps
  (:require [kappa.language :as lang]
            [kappa.misc :as misc]
            [clojure.contrib.condition :as c]
            [clojure.set :as set]))

;;; Activation and Inhibition map

(defn- mix
  "Tries to mix a1 with a2 in one single agent. If it couldn't returns nil."
  [a1 a2 e1-e2]
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

(defn- create-mixed-expr
  "Returns the smallest expression mixing e1 and e2."
  [e1 e2]
  (let [[e1-e2 & left] (lang/pair-exprs e1 e2)]
    (loop [agent-pairs e1-e2,
           ids-map (zipmap (apply concat (map keys left)) (repeatedly #(misc/counter))),
           result (into {} (apply concat
                                  (map (partial map (fn [[id a]] [(ids-map id) a])) left)))]
      (if (empty? agent-pairs)
        (zipmap (keys result)
                (doall (map #(update-in (val %) [:bindings]
                                 (fn [b] (zipmap (keys b) (replace ids-map (vals b))))) result)))

        (let [[oae1 oae2] (first agent-pairs)]
          (if-let [mixed-agent (mix (val oae1) (val oae2) e1-e2)]
            (let [id (misc/counter)]
              (recur (rest agent-pairs)
                     (into ids-map [{(key oae1) id} {(key oae2) id}])
                     (conj result {id mixed-agent})))
          
            (let [id1 (misc/counter), new-oae1 {id1 (val oae1)},
                  id2 (misc/counter), new-oae2 {id2 (val oae2)}]
              (recur (rest agent-pairs)
                     (into ids-map [{(key oae1) id1} {(key oae2) id2}])
                     (into result [new-oae1 new-oae2])))))))))

(defn codomain
  "Returns a map from pairs [oae, site] in p to pairs [oae, site] in e that matches.
  This function requires that the given expressions has accurate information about
  their complexes in the metadata."
  [p e]
  (apply merge ;; don't replace this by into {}! remember that it returns a lot of empty maps
         (let [e-complexes (map (partial lang/subexpr e) (-> e meta :complexes))]
           (for [p-complex (map (partial lang/subexpr p) (-> p meta :complexes))
                 
                 matched-complex (keep (partial lang/match p-complex) e-complexes)
                 
                 ;; for each site in each agent of p-complex
                 [pa-id pa :as p-oae] p-complex, pa-site (-> pa :states keys)
                 
                 :let [ea-id (matched-complex pa-id),  e-oae [ea-id (e ea-id)]]]
             {[p-oae pa-site] [e-oae pa-site]}))))

(defn modified-site-in-mixed-expr-codomain [mss e1 e2]
  (let [;; compute a minimal mixed expression between e1 and e2
        S (lang/with-complexes (create-mixed-expr e1 e2)),
        
        ;; compute the codomains of e1 and e2 in S
        [e1-S e2-S] (map codomain [e1 e2] (repeat S)),
        
        ;; compute the intersection between the sites in the codomains of e1 and e2 in S
        cod (apply set/intersection (map (comp set vals) [e1-S e2-S])),
        
        ;; get back the sites corresponding to that intersection in e1
        dom (set (for [[pa-site ea-site] e1-S
                       :when (cod ea-site)]
                   pa-site))]
    ;; if at least one of the sites in the intersection is a site modified by r
    ;; then r1 activates/inhibits r2
    (some dom mss)))

(defn activates?
  "Tells whether r1 activates r2."
  [r1 r2]
  (boolean
   (modified-site-in-mixed-expr-codomain (-> r1 :action meta :modified-sites) ;; mss
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
  (if (identical? r1 r2)
    false
    (boolean
     (modified-site-in-mixed-expr-codomain (-> r1 :action meta :modified-sites) ;; mss
                                           (:lhs r1) (:lhs r2)))))

(defn inhibition-map
  "Returns a map from each rule r in rules to the rules r inhibits."
  [rules]
  (apply merge
         (for [r1 rules]
           {r1 (filter (partial inhibits? r1) rules)})))


;;; Matching map and Lift map

(defn matching-and-lift-map
  "Compute the matching map and lift map. For reference, see
  'Scalable Simulation of Cellular Signaling Networks', V. Danos, J. Feret,
  W. Fontana, and J. Krivine, 2007."
  [rule-set mixture]
  (->> (for [r rule-set, cr (-> r :lhs meta :complexes)] ; for each pair [r, cr]
         ;; cr and cm are seqs of ids... matchings is a seq of sets of ids
         (let [cr-expr (with-meta (lang/subexpr (:lhs r) cr)
                         {:complexes [cr]})
               aux (keep (fn [cm] (codomain cr-expr (with-meta (lang/subexpr mixture cm)
                                                      {:complexes [cm]})))
                         (-> mixture meta :complexes))
               cods (map (comp (partial map (fn [[[id a] s]] [id s])) vals) aux)
               matchings (map (fn [cr->cm]
                                (into {} (map (fn [[[[id1 a1] s1] [[id2 a2] s2]]]
                                                [id1 id2]) cr->cm))) aux)]
           ;;  matchings (map (comp set (partial map first)) cods)]
           (vector
            ;; for the matching map
            {r {cr (set matchings)}}
            ;; for the lift map
            (for [cod cods, [a-id s] cod]
              {a-id {s [{:rule r, :complex cr, :codomain cod}]}}))))
       (apply map vector) ; put what belongs to the matching-map together in a vector
       ;; and what belongs to the lift-map together in another vector
       ;; note: map could be used for the next thing too in this way: (map #(%1 %2) [f1 f2])
       ((juxt (comp (partial apply merge-with merge) ; if the same rule is found twice in the
                    ;; matching map, merge the maps {cr matchings}, as cr won't appear twice
                    first)
              (comp (partial apply merge-with ; if the same agent is found twice in the lift map,
                             ;; merge the maps associated with its sites using vector and then set
                             ;; (as oppposed to the matching map, sites may appear twice)
                             (partial merge-with concat))
                    (partial apply concat) ;; but first make just one big list of all the maps
                    second)))))

;;; Observables map: like a matching map for observed expressions
(defn obs-map
  "Returns a map from observables (which are expressions) to the complexes they
  match to into the mixture.
  Note: not implemented yet."
  [observables mixture]
  (let [om (for [obs observables]
             [obs (set (filter (comp (partial lang/match obs)
                                     (partial lang/subexpr mixture))
                               (-> mixture meta :complexes)))])]
    (with-meta (into {} om)
      ;; metadata of obs-exprs is used as a simple lift map
      (into {} (for [[obs embs] om
                     emb embs, a emb]
                 [a [obs emb]])))))

