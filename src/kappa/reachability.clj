(ns kappa.reachability
  {:doc ""
   :author "Ricardo Honorato-Zimmer"}
  (:require [kappa.language :as lang]
            [kappa.maps :as maps]
            [clojure.contrib.combinatorics :as comb]
            [clojure.set :as set]))

;;; Common fns

(defn- distinct-exprs [es acc]
  (if (empty? es)
    nil
    (let [[c & rem-es] es]
      (if (some #{c} acc)
        (distinct-exprs rem-es acc)
        (cons c (lazy-seq
                  (distinct-exprs rem-es (cons c acc))))))))

(defn new-id [expr id]
  (let [id2 (lang/get-unique-agent-id)
        nb-ids (filter number? (-> id expr :bindings vals))]
    (reduce (fn [e nb-id]
              (let [nb-site (-> (filter (comp #{id} val) (:bindings (expr nb-id))) first key)]
                (assoc-in e [nb-id :bindings nb-site] id2)))
            (-> expr
                (assoc id2 (expr id))
                (dissoc id))
            nb-ids)))

(defn new-ids [expr]
  (reduce new-id expr (keys expr)))

(defn- update-matching-map [mm css ram]
  (->> (for [[cs executed-rule] css
             cm-expr cs
             activated-rule (ram executed-rule)
             cr (-> activated-rule :lhs meta :complexes)
             :let [cr-expr (lang/subexpr (:lhs activated-rule) cr)
                   matching (lang/match-expr cr-expr cm-expr)]
             :when matching]
         {activated-rule {cr #{(into {} (for [[ar am] matching]
                                          [ar (find cm-expr am)]))}}})
       (apply merge-with (partial merge-with into) mm)))

(defn- matching-map [rules expr]
  (->> (for [r rules
             cr (-> r :lhs meta :complexes)
             cm-expr (map (partial lang/subexpr expr) (-> expr meta :complexes))
             :let [cr-expr (lang/subexpr (:lhs r) cr)
                   matching (lang/match-expr cr-expr cm-expr)]
             :when matching]
         {r {cr #{(into {} (for [[ar am] matching]
                             [ar (find cm-expr am)]))}}})
       (apply merge-with (partial merge-with into))))

;;; Complexes

(defn- produced-complexes [cs mm]
  (for [[r m] mm
        matching (map #(zipmap (keys m) %)
                      (apply comb/cartesian-product (vals m)))
        :when (= (count matching) (-> r :lhs meta :complexes count))
        :let [reactants-expr (into {} (for [[_ m] matching
                                            [_ a] m]
                                        a))
              products-expr (-> ((:action r)
                                 {:mixture reactants-expr}
                                 (into {} (for [[c m] matching]
                                            [c (into {} (for [[ar-id am] m]
                                                          [ar-id (key am)]))])))
                                :mixture
                                lang/with-complexes)
              product-cs (map (partial lang/subexpr products-expr)
                              (-> products-expr meta :complexes))
              new-cs (remove #(some (partial lang/match-expr %) cs) product-cs)]
        :when (seq new-cs)]
    [(map new-ids new-cs) r]))

(defn- gen-complexes
  [cs mm ram]
  (let [ncs+r (into {} (produced-complexes cs mm))
        ncs (apply concat (keys ncs+r))]
    (if (empty? ncs+r)
      nil
      (concat ncs (lazy-seq
                    (gen-complexes (concat cs ncs)
                                   (update-matching-map mm ncs+r ram) ram))))))

(defn reachable-complexes
  "Returns a lazy seq of all the complexes reachable by the system."
  [rules expr]
  (let [cs (map (partial lang/subexpr expr)
                (-> expr meta :complexes))
        distinct-cs (distinct-exprs cs nil)]
    (concat distinct-cs (gen-complexes distinct-cs
                                       (matching-map rules expr)
                                       (maps/activation-map rules)))))

;;; Reactions

;; TODO unify this fn with produced-complexes
(defn- possible-reactions [cs rs mm]
  (for [[r m] mm
        matching (map #(zipmap (keys m) %)
                      (apply comb/cartesian-product (vals m)))
        :when (= (count matching) (-> r :lhs meta :complexes count))
        :let [reactants-expr (-> (into {} (for [[_ m] matching
                                                [_ a] m]
                                            a))
                                 lang/with-complexes)
              products-expr (-> ((:action r)
                                 {:mixture reactants-expr}
                                 (into {} (for [[c m] matching]
                                            [c (into {} (for [[ar-id am] m]
                                                          [ar-id (key am)]))])))
                                :mixture
                                (lang/with-complexes))
              product-cs (map (partial lang/subexpr products-expr)
                              (-> products-expr meta :complexes))
              new-cs (remove #(some (partial lang/match-expr %) cs) product-cs)
              rule-instance (lang/make-rule (:name r) reactants-expr products-expr (:rate r))]
        :when (empty? (filter #(and (lang/match-expr (:lhs rule-instance) (:lhs %))
                                    (lang/match-expr (:rhs rule-instance) (:rhs %))) rs))]
    [(map new-ids new-cs) r rule-instance]))

;; TODO unify this fn with gen-complexes
(defn- gen-rule-instances
  [cs rs mm ram]
  (let [ncs+r+rxn (possible-reactions cs rs mm)
        ncs+r (map (partial take 2) ncs+r+rxn)
        rule-instances (map #(nth % 2) ncs+r+rxn)]
    (if (empty? ncs+r+rxn)
      nil
      (concat rule-instances (lazy-seq
                               (gen-rule-instances (apply concat cs (map first ncs+r+rxn))
                                                   (concat rs rule-instances)
                                                   (update-matching-map mm ncs+r ram) ram))))))

(defn get-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system."
  [rules expr]
  (let [cs (map (partial lang/subexpr expr) (-> expr meta :complexes))]
    (gen-rule-instances (distinct-exprs cs nil) nil
                        (matching-map rules expr)
                        (maps/activation-map rules))))

