(ns kappa.language
  (:use [clojure.contrib.math :only (expt)]
        [clojure.contrib.combinatorics :only (cartesian-product)]
        [kappa.parser :only (parse-agent parse-expression parse-rule)]))

;; TODO todo lo de interp, macros y chamber deberian quedar fuera de
;;      este archivo, cada uno en su propio archivo
;; eso nos ahorra los dos :use de arriba
;; pero todavia no, es mas facil manejar un archivo grande en emacs que varios chicos

;;; Data structures

(defrecord Agent [name states bindings])
;; states is a map from site names (as keywords)
;; to internal site states (as strings)
;; bindings is a map from site names to one
;; of the tags: :free, :unspecified, :semi-link
;; or a reference to the bound agent

;; a complex is a seq of agents
;; an expression is a seq of complexes

(defrecord Rule [name lhs rhs rate])
;; rate is the deterministic kinetic constant

(defmethod print-method Agent [a w]
  (.write w (str (apply str (:name a) "("
                        ;; interface
                        (map (fn [s] (interpose \, (str s "~" ((:states a) s)
                                                        "!" (:name @((:bindings a) s)))))
                             (keys (:states a))))
                 ")")))

(defn count-automorphisms [expr] nil)


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
         (apply concat))))

(defn create-rule [name lhs rhs rate]
  (Rule. name (with-meta lhs {:automorphisms (count-automorphisms lhs)}) rhs rate))

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


;;; Predicates
(defn agent?
  "Check if obj is a Kappa agent."
  [obj]
  (= (type obj) Agent))

(defn complex?
  "Check if obj is a Kappa complex."
  [obj]
  (and (coll? obj)
       (every? #(instance? clojure.lang.IDeref %) obj)
       (every? agent? (map deref obj))))

(defn expression?
  "Check if obj is a Kappa expression."
  [obj]
  (and (coll? obj)
       (every? complex? obj)))

(defn rule?
  "Check if obj is a Kappa rule."
  [obj]
  (= (type obj) Rule))


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


;;; Match
(defn match-dispatch [obj1 obj2]
  (or (and (agent? obj1) (agent? obj2) :agent)
      (and (complex? obj1) (complex? obj2) :complex)))

(defmulti match
  "Check if the given pattern (first argument) matches
  the given agent or complex (second argument)."
  {:arglists '([p a] [p c])}
  match-dispatch)

(defmethod match :agent [p a] ; p stands for pattern, a for agent
  (and (= (:name a) (:name p)) ; the names are the same
       ;; every site mentioned in p is present in a
       (every? #(% (:states a)) (keys (:states p)))
       (every? #(% (:bindings a)) (keys (:bindings p)))
       ;; state values of each site mentioned in p is equal or less specific than those in a
       (let [p-states (apply hash-map
                             (flatten (filter #(not (= (second %) ""))
                                              (:states p))))]
         (every? #(= (p-states %) ((:states a) %)) (keys p-states)))
       ;; binding values of each site mentioned in p is equal or less specific than those in a
       (let [p-bindings (apply hash-map
                               (flatten (filter #(not (= (second %) :unspecified))
                                                (:bindings p))))]
         (every? #(case (p-bindings %)
                    :unspecified true
                    :free (= ((:bindings a) %) :free)
                    (not (= ((:bindings a) %) :free)))
                 (keys p-bindings)))))

(defmethod match :complex [p c] ; p stands for pattern, c for complex
  ;; stores all matchings for every agent in p (lazyly :)
  (let [matchings (map (fn [p] (filter #(match @p @%) c)) p)
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
                                     (map (fn [[site binding]]
                                            (case binding
                                              :unspecified true
                                              :semi-link (not (= ((:bindings @a) site) :free))
                                              :free (= ((:bindings @a) site) :free)
                                              (= ((:bindings @a) site) (match-dict binding))))
                                          (:bindings @pa))))
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

;;; Chambers
;; clash es el numero de veces que ha ocurrido un clash
;; activities y stochastic-kcs deberian ser parte de gillespie
;; Chamber deberia tener tb un matching-map and lift-map (el contrario del matching-map)
;; deberian ser estos dos maps parte de gillespie tb?
;; donde van el ram y el rim?
;; me gustaria que al terminar cada paso de simulacion se pudiese llamar una funcion
;; callback sobre los cambios y/o el estado nuevo de la simulacion
;; esta funcion se le podria pasar a la funcion que haga la iteracion
;; esta funcion podria servir para actualizar el volumen u otras partes del Chamber
(defrecord Chamber [volume rules mixture stochastic-kcs activities clash])

(defn update-volume [chamber new-volume]
  (assoc chamber :volume (with-meta new-volume
                           {:changed? true})))

(defn determ2stoch [volume rule]
  (let [Avogadro 6.022e23
        num-complexes (count (:lhs rule))]
    (/ (* (expt volume (- num-complexes 1))
          Avogadro
          (:rate rule))
       (:automorphisms (meta (:lhs rule))))))

(defn update-stochastic-kcs [chamber]
  (if (:changed? (meta (:volume chamber)))
    (-> chamber
        (assoc :volume (with-meta (:volume chamber) {:changed? false}))
        (assoc :stochastic-kcs (map (partial determ2stoch (:volume chamber))
                                    (:rules chamber))))
    chamber))

(defn update-activities [chamber] nil)

(defn gen-event
  "Generates a new event from chamber."
  [chamber]
  nil)

