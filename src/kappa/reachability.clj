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

(defn- matching-map [rules cs]
  (->> (for [r rules
             cr (-> r :lhs meta :complexes)
             cm-expr cs
             :let [cr-expr (lang/subexpr (:lhs r) cr)
                   matching (lang/match-expr cr-expr cm-expr)]
             :when matching]
         {r {cr #{(vary-meta (lang/with-complexes cm-expr) into {:matching matching})}}})
       (apply merge-with (partial merge-with into))))

(defn- update-matching-map [mm c+rs ram]
  (->> (for [[cm-expr executed-rule] c+rs
             activated-rule (ram executed-rule)
             cr (-> activated-rule :lhs meta :complexes)
             :let [cm-expr (lang/with-complexes cm-expr)
                   cr-expr (lang/subexpr (:lhs activated-rule) cr)
                   matching (lang/match-expr cr-expr cm-expr)]
             :when matching]
         {activated-rule {cr #{(vary-meta cm-expr into {:matching matching})}}})
       (apply merge-with (partial merge-with into) mm)))

;;; Complexes

(defn- get-products [r matching reactants-expr]
  (-> ((:action r) {:mixture reactants-expr} matching)
      :mixture
      lang/with-complexes))

(defn- produced-complexes [cs mm]
  (for [[r m] mm
        matching (map (partial zipmap (keys m)) (apply comb/cartesian-product (vals m)))
        :when (= (count matching) (-> r :lhs meta :complexes count))
        :let [reactants-expr (apply lang/mix-exprs (vals matching))
              matching (zipmap (keys matching) (map (comp :matching meta) (vals matching)))
              products-expr (get-products r matching reactants-expr)
              product-cs (lang/complexes products-expr)
              new-cs (remove #(some (partial lang/match-expr %) cs) product-cs)]
        :when (seq new-cs)]
    [(map new-ids new-cs) r]))

(defn- distinct-complexes [cs+rs]
  (let [m (into {} (for [[cs r] cs+rs, c cs] [c r]))
        distinct-cs (distinct-exprs (keys m) [])]
    (for [c distinct-cs] [c (m c)])))

(defn- gen-complexes
  [cs mm ram]
  (let [c+rs (distinct-complexes (produced-complexes cs mm))]
    (if (seq c+rs)
      (let [new-cs (map first c+rs)]
        ;;(doseq [c new-cs] (println c))
        (concat new-cs (lazy-seq
                         (gen-complexes (concat cs new-cs)
                                        (update-matching-map mm c+rs ram) ram))))
      nil)))

(defn reachable-complexes
  "Returns a lazy seq of all the complexes reachable by the system."
  [rules expr]
  (let [cs (lang/complexes expr)
        distinct-cs (distinct-exprs cs nil)]
    (concat distinct-cs (gen-complexes distinct-cs
                                       (matching-map rules distinct-cs)
                                       (maps/activation-map rules)))))

;;; Reactions

(defn match-rule [r1 r2]
  (and (lang/match-expr (:lhs r1) (:lhs r2))
       (lang/match-expr (:rhs r1) (:rhs r2))))

;; TODO unify this fn with produced-complexes
(defn- possible-reactions [cs rs mm]
  (for [[r m] mm
        matching (map (partial zipmap (keys m)) (apply comb/cartesian-product (vals m)))
        ;; why cartesian-product?... so it'll take one expression complex for the first rule
        ;; complex, one expression complex for the second rule complex and so on
        :when (= (count matching) (-> r :lhs meta :complexes count))
        :let [reactants-expr (apply lang/mix-exprs (vals matching))
              matching (zipmap (keys matching) (map (comp :matching meta) (vals matching)))
              products-expr (get-products r matching reactants-expr)
              product-cs (lang/complexes products-expr)
              ;; we need this for update-matching-map
              new-cs (remove #(some (partial lang/match-expr %) cs) product-cs)
              rule-instance (lang/make-rule (:name r) reactants-expr products-expr (:rate r))]
        :when (not-any? (partial match-rule rule-instance) rs)]
    [(map new-ids new-cs) r rule-instance]))

;; TODO unify this fn with gen-complexes
(defn- gen-rule-instances
  [cs rs mm ram]
  (let [cs+r+rxns (possible-reactions cs rs mm)]
    (if (seq cs+r+rxns)
      (let [c+rs (distinct-complexes (map (partial take 2) cs+r+rxns))
            rxns (map #(nth % 2) cs+r+rxns)]
        ;;(doseq [rxn rxns] (println rxn))
        (concat rxns (lazy-seq
                       (gen-rule-instances (concat cs (map first c+rs))
                                           (concat rs rxns)
                                           (update-matching-map mm c+rs ram) ram))))
      nil)))

(defn get-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system."
  [rules expr]
  (let [cs (lang/complexes expr)
        distinct-cs (distinct-exprs cs nil)]
    (gen-rule-instances distinct-cs nil
                        (matching-map rules distinct-cs)
                        (maps/activation-map rules))))

