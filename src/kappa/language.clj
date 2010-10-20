(ns kappa.language
  (:use [clojure.contrib.combinatorics :only (cartesian-product)]))

;; TODO a branch of the project in which agents doesn't have refs
;; to the agents they link, but instead they have the bond label
;; This would make it easier to work with immutable data.

;;; Data structures

(defrecord Agent [name states bindings])
;; states is a map from site names (as keywords)
;; to internal site states (as strings)
;; bindings is a map from site names to one
;; of the tags: :free, :unspecified, :semi-link
;; or a reference to the bound agent

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
                              (str "!" (:name @b))))))
                   (:states a))]
    (.write w (str (:name a)
                   "(" (apply str (interpose \, sites)) ")"))))

;; a complex is a seq of agents
;; an expression is a seq of complexes

(defrecord Rule [name lhs rhs rate action])
;; rate is the deterministic kinetic constant

;; TODO compile-rule and elementary action functions
;; Las reglas deben contener en su estructura las acciones elementales que realiza
;; Cada accion elemental debe ser una funcion que reciba la mixture y
;; la parte del matching map de cada uno de los complejos del lhs de la regla
;; y debe devolver una nueva mixture y matching map (y lift map?)
;;
;; O tal vez deberia solo tener side-effects y modificar directamente los agentes?
;;
;; Ademas debe contener la informacion de que pares (agente, sitio) modifica,
;; para poder calcular el activation-map e inhibition-map.

(defn elementary-actions [lhs rhs] nil)

;; FIXME this shouldn't be so... the elementary actions should be applied only
;; on the complexes that it modifies. For that reason, the elementary-actions
;; function must return a pair [c act] except for bind, which takes two complexes.
(defn action [lhs rhs]
  (let [eas (elementary-actions lhs rhs)]
    (fn [matchings]
      ((apply juxt eas) matchings))))

(defn count-automorphisms [expr] 1)

(defn create-rule [name lhs rhs rate]
  (Rule. name (with-meta lhs {:automorphisms (count-automorphisms lhs)}) rhs rate
         (action lhs rhs)))


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
       (let [p-states (into {} (filter #(not (= (second %) ""))
                                       (:states p)))]
         (every? #(= (p-states %) ((:states a) %)) (keys p-states)))
       ;; binding values of each site mentioned in p is equal or less specific than those in a
       (let [p-bindings (into {} (filter #(not (= (second %) :unspecified))
                                         (:bindings p)))]
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


;; TODO reachable-complexes and reachable-reactions
(defn reachable-complexes
  "Returns a lazy seq of all the complexes reachable by the system."
  [rule-set initial-state]
  nil)

(defn reachable-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system."
  [rule-set initial-state]
  nil)

