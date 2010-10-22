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
  The returning map has agent-refs as keys and maps from
  site-names to the refs of the bound agents as values."
  [expr refs]
  (let [agents-with-sites (filter (fn [[n [_ iface]]]
                                    (not (= iface :empty-interface)))
                                  (indexed expr)) ; and their indices
        label-map (->> agents-with-sites
                       (map (fn [[n [_ iface]]]
                              (->> iface
                                   ;; filter out the sites that has no bond label
                                   (filter (fn [[_ _ binding]] (string? binding)))
                                   (map (fn [[site-name _ binding]]
                                          {binding [n site-name]}))
                                   (apply merge))))
                       (apply merge-with list))]
    (apply merge-with merge
           (zipmap refs (repeat {})) ; make sure every ref gets a map :)
           (for [[[n1 sn1] [n2 sn2]] (vals label-map)] ; sn stands for site-name
             {(nth refs n1) {sn1 (nth refs n2)},
              (nth refs n2) {sn2 (nth refs n1)}}))))

(defn complex [a]
  (pre-traverse (fn [a]
                  (filter #(instance? clojure.lang.IDeref %)
                          (vals (:bindings @a)))) a))

(defn group-in-complexes [expr]
  (loop [expr expr
         groups []]
    (let [remaining (filter (complement (set (apply concat groups))) expr)]
      (if (empty? remaining) groups
          (recur (rest remaining) (cons (complex (first remaining)) groups))))))

(defn interp-iface
  "Translate the parsed interface into the two maps required to construct a k-agent struct.
  The output is a vector containing the two maps.
  Note: as the neighbourhood information requires knowledge of the other agents in the
  expression, bound agent refs in the :bindings map must be provided externally (in nbs).
  See neighbours function."
  ([iface] (interp-iface iface {})) ; no bound agents
  ([iface nbs] (if (= iface :empty-interface)
                 [{}, {}] ; :states and :bindings are empty maps
                 [(zipmap (map first iface) (map second iface)), ; :states
                  (let [skw (filter #(keyword? (third %)) iface)] ; skw: sites with keywords :P
                    (merge (zipmap (map first skw) (map third skw)) nbs))]))) ; :bindings

(defn interp-agent
  ([a] (interp-agent a {}))
  ([a nbs] (let [[states bindings] (interp-iface (a 1) nbs)] ; (a 1) = agent's interface
             (Agent. (a 0) states bindings)))) ; (a 0) = agent's name

(defn interp-subexpr
  "Convert a parsed sub-expression into a seq of refs with the corresponding agents."
  [subexpr]
  (let [refs (repeatedly (count subexpr) #(ref nil))
        nbs (neighbours subexpr refs)]
    (dosync
     (dorun (map (fn [r a] (ref-set r (interp-agent a (nbs r))))
                 refs subexpr)))
    refs))

(defn interp-expr [expr]
  (let [subexprs (group-parsed-agents expr)]
    (->> (for [[factor subexpr] subexprs]
           (repeatedly factor #(interp-subexpr subexpr)))
         (apply concat)
         (apply concat)
         (group-in-complexes))))

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
;;; To define agents in a simple way
;; TODO this function and its use in def-agents and let-agents need
;;      to be tested in kappa.tests.language
(defn create-agent [a]
  (cond
    (string? a) (interp-agent (parse-agent a))
    (map? a) (Agent. (a :name) (a :states) (a :bindings))
    (coll? a) (Agent. (nth a 0) (nth a 1) (nth a 2))
    :else (throw (Exception. (str (class a) " cannot be cast to kappa.language.Agent")))))

(defn create-agent-in-macro [a]
  (cond
    (string? a) `(interp-agent ~(parse-agent a))
    (map? a) `(Agent. ~(a :name) ~(a :states) ~(a :bindings))
    (coll? a) `(Agent. ~(nth a 0) ~(nth a 1) ~(nth a 2))
    (symbol? a) `(create-agent ~a)
    :else (throw (Exception. (str (class a) " cannot be cast to kappa.language.Agent")))))

(defmacro def-agents
  "Define multiple agents (that can be mutually recursive) in a convenient way.
  Each agent can be defined using a vector, a map, a string or a symbol bound to any of them."
  [& bindings]
  (let [[vars exprs] (apply map list (partition 2 bindings))]
    `(do
       ~@(map (fn [v] `(def ~v (ref nil))) vars)
       (dosync
        ~@(map (fn [v e]
                 `(ref-set ~v ~(create-agent-in-macro e)))
               vars exprs)))))

(defmacro let-agents
  "Like let for agents definitions. See def-agents."
  [bindings & body]
  (let [[locals exprs] (apply map list (partition 2 bindings))]
    `(let [~@(for [x (map (fn [v] `(~v (ref nil))) locals), y x] y)]
       (dosync
        ~@(map (fn [v e]
                 `(ref-set ~v ~(create-agent-in-macro e)))
               locals exprs))
       ~@body)))

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

;; Are def-agents and let-agents useful when we have the other four macros
;; for expressions and rules?

