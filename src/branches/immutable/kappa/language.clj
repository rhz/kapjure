(ns kappa.language
  (:use [kappa.misc :only (pre-traverse indexed)]
        [clojure.contrib.combinatorics :only (cartesian-product)])
  (:require [clojure.contrib.seq-utils :as seq-utils]))

;; Proposal for a new name: Kapjure

;;; Data structures

;;; Agents
(defrecord Agent [name states bindings])
;; states is a map from site names (as keywords)
;; to internal site states (as strings)
;; bindings is a map from site names to one
;; of the tags: :free, :unspecified, :semi-link
;; or the id of the bound agent
;; ids are assigned by the expression

(defmethod print-method Agent [a w]
  (let [sites (map (fn [[site state]]
                     (str site
                          ;; internal state
                          (when-not (= state "")
                            (str "~" state))
                          ;; binding state
                          (let [b ((:bindings a) site)]
                            (case b
                              :unspecified "!?"
                              :free ""
                              :semi-link "!_"
                              (str "!" b)))))
                   (:states a))]
    (.write w (str (:name a)
                   "(" (apply str (interpose \, sites)) ")"))))

;;; Expressions
;; an expression is a map from ids to Agents

(defn get-neighbours
  "Get the subexpression containing the neighbours of a."
  [[id a] expr]
  (for [nb-id (->> a :bindings vals (filter #(number? %)))]
    [nb-id (expr nb-id)]))

(defn complex
  "Returns the ids of the complex to which initial-agent belongs."
  [initial-agent expr]
  (map first (pre-traverse #(get-neighbours % expr) initial-agent)))

(defn get-complexes
  "Returns a seq with the ids of the complexes in expr."
  [expr]
  (if (:complexes (meta expr)) (:complexes (meta expr))
      (loop [expr expr
             groups []]
        (let [remaining (remove #((set (apply concat groups)) (val %)) expr)]
          (if (empty? remaining) groups
              (recur (rest remaining) (cons (complex (first remaining) remaining) groups)))))))

(defn select [ids expr]
  (zipmap ids (map expr ids)))

;; probably only patterns need to know the set of all complexes
(defn make-pattern [expr]
  (with-meta expr {:complexes (get-complexes expr)}))

;;; Rules
(defrecord Rule [name lhs rhs rate action])
;; rate is the deterministic kinetic constant

;; TODO compile-rule and elementary action functions
;; Las reglas deben contener en su estructura las acciones elementales que realiza.
;; Cada accion elemental debe ser una funcion que reciba la mixture y
;; la parte del matching map de cada uno de los complejos del lhs de la regla
;; y debe devolver una nueva mixture y matching map (y lift map?).
;;
;; Ademas action debe entregar la informacion de que pares (agente, sitio) modifica
;; la regla, para poder calcular el activation-map e inhibition-map.

(defn modify-state
  "Returns a copy of agent with site's internal state changed to new-state."
  [agent site new-state]
  (fn [matching chamber] ; matching is a seq of ids
    (assoc-in chamber [:mixture
                       (first (filter #(match agent ((:mixture chamber) %)) matching))
                       :states site] new-state)))

(defn bind-agents
  ""
  [c1 a1 c2 a2]
  (fn [matching chamber]
    chamber))

(defn unbind-agents
  ""
  [c a1 a2]
  (fn [matching chamber]
    chamber))

(defn create
  ""
  [a]
  (fn [matching chamber]
    chamber))

(defn delete
  ""
  [a]
  (fn [matching chamber]
    chamber))

(defn elementary-actions [lhs rhs]
  (let [names (map #(set (map (comp :name val) %)) [lhs rhs])
        [lhs-agents-by-name rhs-agents-by-name] ; destructuring
        (map (fn [expr names]
               (merge (for [name names]
                        {name (indexed (filter #(= name (-> % val :name)) expr))})))
             [lhs rhs] names)
        ;; skeleton
        [created-agents rhs-agents-by-name] (seq-utils/separate (constantly nil) ; maybe group-by is better
                                                                rhs-agents-by-name)
        [removed-agents lhs-agents-by-name] (seq-utils/separate (constantly nil)
                                                                lhs-agents-by-name)
        modified-states nil
        bound-agents nil
        unbound-agents nil]
    (concat (map create created-agents)
            (map delete removed-agents)
            (map modify-state modified-states)
            (map bind-agents bound-agents)
            (map unbind-agents unbound-agents))))

(defn action [lhs rhs]
  (fn [chamber matching]
    ((apply comp (map #(partial % matching)
                      (elementary-actions lhs rhs)))
     chamber)))

(defn count-automorphisms [expr] 1)

(defn create-rule [name lhs rhs rate]
  (Rule. name (make-pattern (with-meta lhs {:automorphisms (count-automorphisms lhs)}))
         (make-pattern rhs) rate (action lhs rhs)))


;;; Predicates
(defn agent?
  "Check if obj is a Kappa agent."
  [obj]
  (instance? Agent obj))

(defn expression?
  "Check if obj is a Kappa expression."
  [obj]
  (and (map? obj)
       (every? number? (keys obj))
       (every? agent? (vals obj))))

(defn rule?
  "Check if obj is a Kappa rule."
  [obj]
  (instance? Rule obj))


;;; Match
(defn- match-dispatch [obj1 obj2]
  (or (and (agent? obj1) (agent? obj2) :agent)
      (and (expression? obj1) (expression? obj2) :expression)))

(defmulti match
  "Check if the given pattern (first argument) matches
  the given agent or expression (second argument)."
  {:arglists '([p a] [p c])}
  match-dispatch)

(defmethod match :agent [p a] ; p stands for pattern, a for agent
  (and (= (:name a) (:name p)) ; the names are the same
       ;; every site mentioned in p is present in a
       (every? #((:states a) %) (keys (:states p)))
       (every? #((:bindings a) %) (keys (:bindings p)))
       ;; state values of each site mentioned in p is equal or less specific than those in a
       (let [p-states (into {} (remove #(= (val %) "") (:states p)))]
         (every? #(= (p-states %) ((:states a) %)) (keys p-states)))
       ;; binding values of each site mentioned in p is equal or less specific than those in a
       (let [p-bindings (into {} (remove #(= (val %) :unspecified) (:bindings p)))]
         (every? #(case (p-bindings %)
                    :unspecified true
                    :free (= ((:bindings a) %) :free)
                    (not (= ((:bindings a) %) :free)))
                 (keys p-bindings)))))

(defmethod match :expression [p e] ; p stands for pattern, e for expression
  ;; stores all matchings for every agent in p (lazyly :)
  (let [matchings (map (fn [p] (filter #(match p %) (vals e))) (vals p))
        match-comb (apply cartesian-product matchings) ; matching combinations
        n (count p)]
    ;; try every combination of matchings checking if the neighbours corresponds
    (loop [left match-comb]
      (if (empty? left)
        false
        (let [match-dict (zipmap (vals p) (first left))]
          ;; the neighbours of each agent in the pattern must match
          ;; the neighbours of each agent in the expression
          (or (and (= (count match-dict) n)
                   (every? (fn [[pa a]] ; iterates over every matching pa => a
                             (every? true? ; iterates over every binding of pa
                                     (map (fn [[site binding]]
                                            (case binding
                                              :unspecified true
                                              :semi-link (not (= ((:bindings a) site) :free))
                                              :free (= ((:bindings a) site) :free)
                                              ;; FIXME this can be optimized using ids
                                              (= (e ((:bindings a) site))
                                                 (match-dict (p binding)))))
                                          (:bindings pa))))
                           match-dict))
              (recur (rest left))))))))


;; TODO reachable-complexes and reachable-reactions
(defn reachable-complexes
  "Returns a lazy seq of all the complexes reachable by the system."
  [rule-set initial-state]
  nil)

(defn reachable-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system."
  [rule-set initial-state]
  nil)

