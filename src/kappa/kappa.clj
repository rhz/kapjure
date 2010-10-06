(ns kappa.kappa
  (:use [clojure.contrib.combinatorics :only (cartesian-product)]
        [kappa.parser :only (parse-agent)]))

;;; Data structures

(defstruct k-agent :name :states :bindings)
;; :states is a map from site names (as keywords)
;; to internal site states (as strings)
;; :bindings is a map from site names to one
;; of the tags: :free, :unspecified, :semi-link
;; or a reference to the bounded agent

;; a complex is a seq of agents
;; an expression is a seq of complexes

(defstruct rule :name :lhs :rhs :rate)


;;; Interpret: convert the parsed string into the data structures above
(defn interp [expr]
  nil)


;;; Predicates
(defn agent?
  "Check if obj is a Kappa agent"
  [obj]
  (and (:name obj) (:states obj) (:bindings obj) true))

(defn complex?
  "Check if obj is a Kappa complex"
  [obj]
  (and (coll? obj)
       (every? agent? obj)))

(defn expression?
  "Check if obj is a Kappa expression"
  [obj]
  (and (coll? obj)
       (every? complex? obj)))

(defn rule?
  "Check if obj is a Kappa rule"
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
(defn get-agent-defs [bindings]
  ;; vars are in odd positions and exprs in even positions
  (->> (for [[v e] (partition 2 bindings)]
         [v (cond
              ;; transform the agent's extended definition
              ;; form (maps) into the compact form (vector)
              (map? e) [(:name e) (:states e) (:bindings e)]
              (string? e) (interp (parse-agent e))
              :else e)])
       (apply map list)))

(defmacro def-agents [& bindings]
  (let [[vars exprs] (get-agent-defs bindings)]
    `(do
       ~@(map (fn [v] `(def ~v (ref nil))) vars)
       (dosync
        ~@(map (fn [v e]
                 `(ref-set ~v ~(if (or (symbol? e) (agent? e)) e
                                   `(struct k-agent ~(e 0) ~(e 1) ~(e 2)))))
               vars exprs)))))

(defmacro let-agents [bindings & body]
  (let [[vars exprs] (get-agent-defs bindings)]
    `(let [~@(for [x (map (fn [v] `(~v (ref nil))) vars), y x] y)]
       (dosync
        ~@(map (fn [v e]
                 `(ref-set ~v ~(if (or (symbol? e) (agent? e)) e
                                   `(struct k-agent ~(e 0) ~(e 1) ~(e 2)))))
               vars exprs))
       ~@body)))


;;; Functions
(defn modify-state
  "Returns a copy of agent with site's internal state changed to new-state"
  [agent site new-state]
  (assoc-in agent [:states site] new-state))

;; TODO bind agents? unbind them?
(defn bind-agents [c1 a1 c2 a2] nil)
;; bind-agents must return one complex
(defn unbind-agents [c a1 a2] nil)
;; unbind-agents must return two complexes

(defn kype
  "Returns a keyword representing the type of obj.
  The possible keywords are: :agent, :complex, :expression and :rule
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
  the given agent or complex (second argument)"
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
  "Returns a lazy seq of all the complexes reachable by the system"
  [rule-set initial-state]
  nil)

(defn reachable-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system"
  [rule-set initial-state]
  nil)
