(ns kappa.language
  (:use [clojure.contrib.combinatorics :only (cartesian-product)]
        [kappa.parser :only (parse-agent parse-expression parse-rule)]))

;;; Data structures

(defstruct k-agent :name :states :bindings)
;; :states is a map from site names (as keywords)
;; to internal site states (as strings)
;; :bindings is a map from site names to one
;; of the tags: :free, :unspecified, :semi-link
;; or a reference to the bound agent

;; a complex is a seq of agents
;; an expression is a seq of complexes

(defstruct rule :name :lhs :rhs :rate)


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

(defn indexed [s]
  (map vector (iterate inc 0) s))

(def third #(nth % 2))

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
  ([a] (apply struct k-agent (a 0) ; (a 0) = agent's name
              (interp-iface (a 1)))) ; (a 1) = agent's interface
  ([a nbs] (apply struct k-agent (a 0)
                  (interp-iface (a 1) nbs))))

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
         (apply concat))))

(defn interp-rule [r]
  (if (= (r :rule-type) :bidirectional-rule)
    [(struct rule (r :name) (interp-expr (r :lhs)) (interp-expr (r :rhs)) (r :rate))
     (struct rule (r :name) (interp-expr (r :rhs)) (interp-expr (r :lhs)) (r :rate))]
    [(struct rule (r :name) (interp-expr (r :lhs)) (interp-expr (r :rhs)) (r :rate))]))

(defn interp
  "Convert the parsed string into the corresponding clj-kappa data structures."
  [expr]
  (cond
    (map? expr) (interp-rule expr)
    (string? (first expr)) (interp-agent expr)
    :else (interp-expr expr)))


;;; Predicates
(defn agent?
  "Check if obj is a Kappa agent."
  [obj]
  (and (:name obj) (:states obj) (:bindings obj) true))

(defn complex?
  "Check if obj is a Kappa complex."
  [obj]
  (and (coll? obj)
       (every? agent? obj)))

(defn expression?
  "Check if obj is a Kappa expression."
  [obj]
  (and (coll? obj)
       (every? complex? obj)))

(defn rule?
  "Check if obj is a Kappa rule."
  [obj]
  (and (:lhs obj) (:rhs obj) (:rate obj) true))


;;; Macros
;;; To define agents in a simple way
;; TODO the only thing that doesnt work yet is to define a variable (with let or def)
;; for a map or vector and then use that variable to define agents. For example:
;; (def agent-defining-map {:name "a" :states {} :bindings {}})
;; (def-agents a1 agent-defining-map)
;; TODO define rules and expressions from strings at compile-time, if it's possible,
;; or run-time otherwise.
(defn get-agent-defs
  "Process the bindings part of def-agents and let-agents."
  [bindings]
  ;; vars are in odd positions and exprs in even positions
  (->> (for [[v e] (partition 2 bindings)]
         [v (cond
              ;; transform the agent's extended definition
              ;; form (maps) into the compact form (vector)
              (map? e) [(:name e) (:states e) (:bindings e)]
              (string? e) (interp-agent (parse-agent e))
              :else e)])
       (apply map list)))

;; TODO this function and its use in def-agents and let-agents need
;;      to be tested in kappa.tests.language
(defn create-agent [a]
  (cond
    (string? a) (interp-agent (parse-agent a))
    (map? a) (struct k-agent (a :name) (a :states) (a :bindings))
    (coll? a) (struct k-agent (nth a 0) (nth a 1) (nth a 2))
    :else (throw (Exception. (str (class a) " cannot be cast to kappa.language/k-agent")))))

(defmacro def-agents
  "Define multiple agents in a convenient way."
  [& bindings]
  (let [[vars exprs] (get-agent-defs bindings)]
    `(do
       ~@(map (fn [v] `(def ~v (ref nil))) vars)
       (dosync
        ~@(map (fn [v e]
                 `(ref-set ~v ~(cond
                                 (agent? e) e
                                 (symbol? e) `(create-agent ~e)
                                 :else `(struct k-agent ~(e 0) ~(e 1) ~(e 2)))))
               vars exprs)))))

(defmacro let-agents
  ""
  [bindings & body]
  (let [[vars exprs] (get-agent-defs bindings)]
    `(let [~@(for [x (map (fn [v] `(~v (ref nil))) vars), y x] y)]
       (dosync
        ~@(map (fn [v e]
                 `(ref-set ~v ~(cond
                                 (agent? e) e
                                 (symbol? e) `(create-agent ~e)
                                 :else `(struct k-agent ~(e 0) ~(e 1) ~(e 2)))))
               vars exprs))
       ~@body)))


;;; Functions
(defn modify-state
  "Returns a copy of agent with site's internal state changed to new-state."
  [agent site new-state]
  (assoc-in agent [:states site] new-state))

;; TODO bind agents? unbind them?
(defn bind-agents [c1 a1 c2 a2] nil)
;; bind-agents must return one complex
(defn unbind-agents [c a1 a2] nil)
;; unbind-agents must return two complexes

(defn kype
  "Returns a keyword representing the type of obj.
  The possible keywords are: :agent, :complex, :expression and :rule.
  kype is a contraction between Kappa and type."
  [obj]
  (cond
    (agent? obj) :agent
    (complex? obj) :complex
    (expression? obj) :expression
    (rule? obj) :rule))


;;; Match
(defn match-dispatch [obj1 obj2]
  (when-let [type-obj1 (kype obj1)]
    (when (= type-obj1 (kype obj2))
      type-obj1)))

(defmulti match
  "Check if the given pattern (first argument) matches
  the given agent or complex (second argument)."
  {:arglists '([p a] [p c])}
  match-dispatch)

(defmethod match :agent [p a] ; p stands for pattern, a for agent
  (and (= (a :name) (p :name)) ; the names are the same
       ;; every site mentioned in p is present in a
       (every? #(% (a :states)) (keys (p :states)))
       (every? #(% (a :bindings)) (keys (p :bindings)))
       ;; state values of each site mentioned in p is equal or less specific than those in a
       (let [p-states (apply hash-map
                             (flatten (filter #(not (= (second %) ""))
                                              (p :states))))]
         (every? #(= (p-states %) ((a :states) %)) (keys p-states)))
       ;; binding values of each site mentioned in p is equal or less specific than those in a
       (let [p-bindings (apply hash-map
                               (flatten (filter #(not (= (second %) :unspecified))
                                                (p :bindings))))]
         (every? #(case (p-bindings %)
                    :unspecified true
                    :free (= ((a :bindings) %) :free)
                    (not (= ((a :bindings) %) :free)))
                 (keys p-bindings)))))

(defmethod match :complex [p c] ; p stands for pattern, c for complex
  ;; stores all matchings for every agent in p (lazyly :)
  (let [matchings (map (fn [p] (filter #(match p %) c)) p)
        match-comb (apply cartesian-product matchings) ; matching combinations
        n (count p)]
    ;; try every combination of matchings checking if the neighbours corresponds
    (loop [left match-comb]
      (if (empty? left)
        false
        (let [match-dict (zipmap p (first left))]
          ;; the neighbours of every agent in the pattern must match the
          ;; neighbours of every agent in the agent
          (or (and (= (count match-dict) n)
                   (every? (fn [[pa a]] ; iterates over every matching pa => a
                             (every? true?
                                     (map (fn [[site nb]] ; nb: neighbour of pa
                                            (case nb
                                              :unspecified true
                                              :semi-link (not (= ((a :bindings) site) :free))
                                              :free (= ((a :bindings) site) :free)
                                              (= @((a :bindings) site) (match-dict @nb))))
                                          (pa :bindings))))
                           match-dict))
              (recur (rest left))))))))


;;; Rules
;; TODO compile-rule and elementary action functions

;; TODO reachable-complexes and reachable-reactions
(defn reachable-complexes
  "Returns a lazy seq of all the complexes reachable by the system."
  [rule-set initial-state]
  nil)

(defn reachable-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system."
  [rule-set initial-state]
  nil)
