(ns kappa.reachability
  {:doc ""
   :author "Ricardo Honorato-Zimmer"}
  (:require [kappa.language :as lang]
            [kappa.maps :as maps]
            [clojure.contrib.combinatorics :as comb]
            [clojure.set :as set]))

;;; Common fns

(defn distinct-exprs [es seen]
  (if (empty? es)
    nil
    (let [[c & rem-es] es]
      (if (some (partial lang/match-expr c) seen)
        (distinct-exprs rem-es seen)
        (cons c (lazy-seq
                  (distinct-exprs rem-es (cons c seen))))))))

(defn- replace-bound-agent-id [old-nb-id new-nb-id expr id]
  (let [[[site _]] (filter (comp #{old-nb-id} val) (:bindings (expr id)))]
    (assoc-in expr [id :bindings site] new-nb-id)))

(defn new-id [expr id]
  (let [new-id (lang/get-unique-agent-id)
        nb-ids (filter number? (-> id expr :bindings vals))
        new-expr (reduce (partial replace-bound-agent-id id new-id) expr nb-ids)]
    (-> new-expr
        (assoc new-id (new-expr id))
        (dissoc id)
        (vary-meta update-in [:complexes]
                   (partial map (comp set (partial replace {id new-id})))))))

(defn new-ids [expr]
  (reduce new-id expr (keys expr)))

(defn update-matching-map [mm rules cs]
  (->> (for [cm-expr (map lang/with-complexes cs)
             r rules
             cr (-> r :lhs meta :complexes)
             :let [cr-expr (lang/subexpr (:lhs r) cr)
                   matching (lang/match-expr cr-expr cm-expr)]
             :when matching]
         {r {cr #{(vary-meta cm-expr into {:matching matching})}}})
       (apply merge-with (partial merge-with into) mm)))

;;; Complexes

(defn- get-products [r matching reactants-expr]
  (-> ((:action r) {:mixture reactants-expr} matching)
      :mixture
      lang/with-complexes))

(defn- produced-complexes [cs mm]
  (->> (for [[r m] mm
             matching (map (partial zipmap (keys m)) (apply comb/cartesian-product (vals m)))
             :when (= (count matching) (-> r :lhs meta :complexes count))
             :let [reactants-expr (apply lang/mix-exprs (vals matching))
                   matching (zipmap (keys matching) (map (comp :matching meta) (vals matching)))
                   products-expr (get-products r matching reactants-expr)
                   product-cs (lang/complexes products-expr)
                   new-cs (remove #(some (partial lang/match-expr %) cs) product-cs)]
             :when (seq new-cs)]
         (map new-ids new-cs))
       (apply concat)))

(defn- gen-complexes [rules cs mm]
  (let [new-cs (distinct-exprs (produced-complexes cs mm) [])]
    (if (seq new-cs)
      (concat new-cs (lazy-seq
                       (gen-complexes rules (concat cs new-cs)
                                      (update-matching-map mm rules new-cs))))
      nil)))

(defn reachable-complexes
  "Returns a lazy seq of all the complexes reachable by the system."
  [rules expr]
  (let [distinct-cs (distinct-exprs (lang/complexes expr) [])]
    (concat distinct-cs (gen-complexes rules distinct-cs
                                       (update-matching-map {} rules distinct-cs)))))

;;; Reactions

(defn match-rule [r1 r2]
  (and (lang/match-expr (:lhs r1) (:lhs r2))
       (lang/match-expr (:rhs r1) (:rhs r2))))

;; TODO unify this fn with produced-complexes
(defn- possible-reactions [cs rxns mm]
  (for [[r m] mm
        matching (map (partial zipmap (keys m)) (apply comb/cartesian-product (vals m)))
        ;; we use the cartesian-product so it'll take one expression complex for the first rule
        ;; complex, one expression complex for the second rule complex and so on
        :when (= (count matching) (-> r :lhs meta :complexes count))
        :let [reactants-expr (apply lang/mix-exprs (vals matching))
              matching (zipmap (keys matching) (map (comp :matching meta) (vals matching)))
              products-expr (get-products r matching reactants-expr)
              product-cs (lang/complexes products-expr)
              ;; we need this for update-matching-map
              new-cs (remove #(some (partial lang/match-expr %) cs) product-cs)
              rule-instance (lang/make-rule (:name r) reactants-expr products-expr (:rate r))]
        :when (not-any? (partial match-rule rule-instance) rxns)]
    [(map new-ids new-cs) rule-instance]))

;; TODO unify this fn with gen-complexes
(defn- gen-rule-instances [rules cs rxns mm]
  (let [cs+rxns (possible-reactions cs rxns mm)]
    (if (seq cs+rxns)
      (let [new-cs (distinct-exprs (mapcat first cs+rxns) [])
            new-rxns (map second cs+rxns)]
        (concat new-rxns (lazy-seq
                           (gen-rule-instances rules (concat cs new-cs) (concat rxns new-rxns)
                                               (update-matching-map mm rules new-cs)))))
      nil)))

(defn get-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system."
  [rules expr]
  (let [distinct-cs (distinct-exprs (lang/complexes expr) [])]
    (gen-rule-instances rules distinct-cs []
                        (update-matching-map {} rules distinct-cs))))

