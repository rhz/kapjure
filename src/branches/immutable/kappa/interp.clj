(ns kappa.interp
  (:use kappa.misc
        [kappa.language :only (create-rule)]
        [kappa.parser :only (parse-agent parse-expression parse-rule)])
  (:import (kappa.language Agent Rule)))

;;; Interpret: convert the parsed string into the data structures above
;;; See kappa.parser
(defn group-parsed-agents
  "Group the parsed agents for further processing in a regular way.
  The output is a seq of subexpressions and their factors, i.e., a seq of [factor subexpr]."
  [expr]
  (loop [[x & xs] expr
         non-grouped []
         grouped []]
    (cond
      (nil? x) (cons [1 non-grouped] grouped)
      (number? (first x)) (recur xs non-grouped
                                 (cons (group-parsed-agents x) grouped)) ; subexpr + factor
      (coll? (first x)) (recur xs (concat x non-grouped) grouped) ; agent + factor
      :else (recur xs (cons x non-grouped) grouped)))) ; agent alone

(defn neighbours
  "Creates a map representing the neighbourhood of each symbol.
  The returning map has ids as keys and maps from site-names to
  the ids of the bound agents as values."
  [ids2agents]
  (let [agents-with-sites (filter (fn [[_ [_ iface]]]
                                    (not (= iface :empty-interface)))
                                  ids2agents)
        label-map (->> agents-with-sites
                       (map (fn [[id [_ iface]]]
                              (->> iface
                                   ;; filter out the sites that has no bond label
                                   (filter (fn [[_ _ binding]] (string? binding)))
                                   (map (fn [[site-name _ binding]]
                                          {binding [id site-name]}))
                                   (apply merge))))
                       (apply merge-with list))]
    (apply merge-with merge
           (zipmap (keys ids2agents) {})
           (for [[[id1 sn1] [id2 sn2]] (vals label-map)] ; sn stands for site-name
             {id1 {sn1 id2}, id2 {sn2 id1}}))))

(defn interp-iface
  "Translate the parsed interface into the two maps required to construct a k-agent struct.
  The output is a vector containing the two maps.
  Note: as the neighbourhood information requires knowledge of the other agents in the
  expression, bound agent refs in the :bindings map must be provided externally (in nbs).
  See neighbours function."
  ([iface] (interp-iface iface {})) ; no bound agents
  ([iface nbs] (if (= iface :empty-interface) [{} {}] ; :states and :bindings are empty maps
                   [(zipmap (map first iface) (map second iface)), ; :states
                    (let [sk (filter (comp keyword? third) iface)] ; sk: sites with keywords :P
                      (merge (zipmap (map first sk) (map third sk)) nbs))]))) ; :bindings

(defn interp-agent
  ([a] (interp-agent a {}))
  ([a nbs] (let [[states bindings] (interp-iface (a 1) nbs)] ; (a 1) = agent's interface
             (Agent. (a 0) states bindings)))) ; (a 0) = agent's name

(defn interp-subexpr
  "Convert a parsed sub-expression into a seq of refs with the corresponding agents."
  [subexpr]
  (let [ids (repeatedly (count subexpr) #(counter))
        ids2agents (zipmap ids subexpr)
        nbs (neighbours ids2agents)]
    (into {} (map (fn [[id a]] [id (interp-agent a (nbs id))])
                  ids2agents))))

(defn interp-expr [expr]
  (let [subexprs (group-parsed-agents expr)]
    (->> (for [[factor subexpr] subexprs]
           (repeatedly factor #(interp-subexpr subexpr)))
         (apply concat) ;; remove lists introduced by repeatedly
         (apply merge)))) ;; merge subexpressions

(defn interp-rule [r]
  (if (= (r :rule-type) :bidirectional-rule)
    [(create-rule (:name r) (interp-expr (:lhs r)) (interp-expr (:rhs r)) (:rate r))
     (create-rule (:name r) (interp-expr (:rhs r)) (interp-expr (:lhs r)) (:rate r))]
    [(create-rule (:name r) (interp-expr (:lhs r)) (interp-expr (:rhs r)) (:rate r))]))

(defn interp
  "Convert the parsed string into the corresponding clj-kappa data structures."
  [expr]
  (cond
    (map? expr) (interp-rule expr)
    (string? (first expr)) (interp-agent expr)
    :else (interp-expr expr)))


;;; Macros
;;; To define expressions and rules in a simple way
(defmacro def-expressions
  [& bindings]
  (let [[vars exprs] (apply map list (partition 2 bindings))]
    `(do ~@(map (fn [v e] `(def ~v (interp-expr (parse-expression ~e))))
                vars exprs))))

(defmacro let-expressions
  [bindings & body]
  (let [[locals exprs] (apply map list (partition 2 bindings))]
    `(let [~@(for [x (map (fn [v e] `(~v (interp-expr (parse-expression ~e))))
                          locals exprs)
                   y x] y)]
       ~@body)))

(defmacro def-rules
  [& bindings]
  (let [[vars exprs] (apply map list (partition 2 bindings))]
    `(do ~@(map (fn [v e] `(let [r# (interp-rule (parse-rule ~e))]
                             (def ~v (nth r# 0))
                             (def ~(symbol (str v "_op")) (nth r# 1 nil))))
                vars exprs))))

(defmacro let-rules
  [bindings & body]
  (let [[locals exprs] (apply map list (partition 2 bindings))]
    `(let [~@(for [x (map (fn [v e] `[r# (interp-rule (parse-rule ~e))
                                      ~v (nth r# 0)
                                      ~(symbol (str v "_op")) (nth r# 1 nil)])
                          locals exprs)
                   y x] y)]
       ~@body)))

