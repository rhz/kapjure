(ns kappa.language
  (:use [kappa.misc :only (pre-traverse indexed counter xor)]
        [clojure.contrib.combinatorics :only (cartesian-product)])
  (:require [clojure.set :as set]))

;;;; Proposal for a new name: Kapjure

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

(defn agent?
  "Check if obj is a Kappa agent."
  [obj]
  (instance? Agent obj))


;;; Expressions
;; an expression is a map from ids to Agents
;; a particular kind of expression is worth a mention: one-agent-expression or oae
;; as it names suggests, they contain just one agent
;; TODO expressions could be compiled to objects using defrecord/reify for faster access

(defn get-state [oae s]
  (-> oae key :states s))

(defn get-binding [oae s]
  (-> oae key :bindings s))

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
        (let [remaining (into {} (remove #((set (apply concat groups)) (val %)) expr))]
          (if (empty? remaining) groups
              (recur (rest remaining) (conj groups (complex (first remaining) remaining))))))))

(defn subexpr
  "Gets the subexpression for the given ids and expression expr."
  [ids expr]
  (zipmap ids (map expr ids)))

;; probably only patterns need to know the set of all complexes
(defn make-pattern [expr]
  (with-meta expr {:complexes (get-complexes expr)}))

(defn expression?
  "Check if obj is a Kappa expression."
  [obj]
  (and (map? obj)
       (every? number? (keys obj))
       (every? agent? (vals obj))))


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
  (let [matchings (map (fn [p] (filter #(match p (val %)) e)) (vals p))
        match-comb (apply cartesian-product matchings) ; matching combinations
        n (count p)]
    ;; try every combination of matchings checking if the neighbours corresponds
    (loop [left match-comb]
      (if (empty? left)
        nil
        (let [match-dict (zipmap (vals p) (vals (first left)))]
          ;; the neighbours of each agent in the pattern must match
          ;; the neighbours of each agent in the expression
          (or (if (and (= (count match-dict) n)
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
                (zipmap (keys p) (keys (first left))))
              (recur (rest left))))))))


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
  "Returns a function that modifies the state of site s in agent
  ((matching c) a) to new-state."
  [c a s new-state] ; c is a seq of lhs agent ids. a is a lhs agent id.
  ;; matching is a map from lhs complex to a map from lhs agents ids to mixture agents ids
  (fn [chamber matching]
    (let [ma ((matching c) a)]
      (-> chamber
          (assoc-in [:mixture ma :states s] new-state)
          (vary-meta (fn [m] (update-in m [:modified-agents]
                                        #(conj % [(-> chamber :mixture ma) s]))))))))

(defn bind-agents
  "Returns a function that binds agents a1 and a2 through
  sites s1 and s2, respectively."
  [c1 a1 s1 c2 a2 s2]
  (fn [chamber matching]
    (let [ma1 ((matching c1) a1), ma2 ((matching c2) a2)]
      (-> chamber
          (assoc-in [:mixture ma1 :bindings s1] ma2)
          (assoc-in [:mixture ma2 :bindings s2] ma1)
          (vary-meta (fn [m] (update-in m [:modified-agents]
                                        #(conj (conj % [(-> chamber :mixture ma1) s1])
                                               [(-> chamber :mixture ma2) s2]))))))))

(defn unbind-agents
  "Returns a function that unbinds agents a1 and a2."
  [c1 a1 s1 c2 a2 s2]
  (fn [chamber matching]
    (let [ma1 ((matching c1) a1), ma2 ((matching c2) a2)]
      (-> chamber
          (assoc-in [:mixture ma1 :bindings s1] :free)
          (assoc-in [:mixture ma2 :bindings s2] :free)
          (vary-meta (fn [m] (update-in m [:modified-agents]
                                        #(conj (conj % [(-> chamber :mixture ma1) s1])
                                               [(-> chamber :mixture ma2) s2]))))))))

(defn create
  "Returns a function that creates an agent a in chamber's mixture.
  Agent a must not be bound."
  [a]
  (if (every? #(or (= % :free) (= % :unspecified)) (:bindings a)) ; a can't be bound
    ;; TODO should I allow to create bound agents? what if you want to create a complex?
    (fn [chamber _]
      (-> chamber
          (assoc-in [:mixture (counter)] a)
          (vary-meta (fn [m] (update-in m [:added-agents] #(conj % a))))))
    (throw (Exception. "Agents created by rules can't be bound."))))

(defn destroy
  "Returns a function that destroys agent a in chamber's mixture."
  [c a]
  (fn [chamber matching]
    (let [ma ((matching c) a),
          nbs (filter number? (-> chamber :mixture ma :bindings vals))]
      (reduce (fn [chamber nb]
                (let [site (first (filter (comp #{ma} val) (:bindings nb)))]
                  (-> chamber
                      (assoc-in [:mixture nb :bindings site] :free)
                      (vary-meta (fn [m] (update-in m [:modified-agents] #(conj % [nb site])))))))
              (-> chamber
                  (update-in [:mixture] #(dissoc % ma))
                  (vary-meta (fn [m] (update-in m [:removed-agents] #(conj % ma)))))
              nbs))))

(defn pairing-lhs-rhs
  "Returns a seq of three things:
  1. A seq of pairs [lhs oae, rhs oae] which will be considered to be equivalent.
  2. A seq of rhs oaes that doesn't have a counterpart in lhs. These agents will be
     created by the rule.
  3. A seq of lhs oaes that doesn't have a counterpart in rhs. These agents will be
     destroyed by the rule."
  [lhs rhs]
  (let [names (map #(set (map (comp :name val) %)) [lhs rhs])
        [lhs-agents-by-name rhs-agents-by-name]
        (map (fn [expr names]
               (apply merge (for [name names]
                              {name (filter #(= name (-> % val :name)) expr)})))
             [lhs rhs] names)]
    (->> (for [name (apply set/union names)]
           (let [lhs-agents (lhs-agents-by-name name), n (count lhs-agents),
                 rhs-agents (rhs-agents-by-name name), m (count rhs-agents)]
             (cond
               (= n m) [(map vector lhs-agents rhs-agents) nil nil]
               
               (< n m) (let [[rhs-agents created-agents]
                             (split-at (count lhs-agents) rhs-agents)]
                         [(map vector lhs-agents rhs-agents) created-agents nil])
               
               :else   (let [[lhs-agents removed-agents]
                             (split-at (count rhs-agents) lhs-agents)]
                         [(map vector lhs-agents rhs-agents) nil removed-agents]))))
         (apply map vector)
         (map (partial apply concat)))))

(defn get-modified-sites [lhs-rhs]
  (->> (for [agent-pair lhs-rhs]
         (let [[[lhs-id {lstates :states, lbindings :bindings} :as lhs-agent]
                [rhs-id {rstates :states, rbindings :bindings}]] agent-pair]
           (for [site (set/union (-> lstates keys set)
                                 (-> rstates keys set))]
             (let [ls (lstates site), lb (lbindings site),
                   rs (rstates site), rb (rbindings site)]
               (merge
                (when-not (or (= ls rs) (nil? rs))
                  {:modified [lhs-agent site rs]})
                (when (and (not (number? lb)) (number? rb))
                  {:bound {(ffirst (filter (comp #{rb} key second) lhs-rhs)) [lhs-agent site]}})
                (when (and (number? lb) (not (number? rb)))
                  {:unbound {lb [lhs-agent site]}}))))))
       (apply concat)
       (apply merge-with vector)))

(defn- get-modify-state-fns [{mss :modified} lhs]
  (map (fn [[lhs-agent site new-state]]
         (modify-state (complex lhs-agent lhs) lhs-agent site new-state))
       mss))

(defn- get-bind-agents-fns [{bss :bound} lhs]
  (let [bss (into {} (for [[a2 [a1 s1]] bss]
                       {#{a1 a2} [s1 (-> a1 bss second)]}))]
    (for [[[a1 a2] [s1 s2]] bss]
      (bind-agents (complex a1 lhs) a1 s1 (complex a2 lhs) a2 s2))))

(defn- get-unbind-agents-fns [{uss :unbound} lhs]
  (get-bind-agents-fns uss lhs))

(defn- modified-bound-and-unbound
  "Look for differences between lhs and rhs. It receives a seq of pairs [lhs oae, rhs oae]
  and returns a seq of functions modify-state, bind-agents and unbind-agents."
  [lhs-rhs lhs rhs]
  (->> (let [done-sites (atom [])]
         (for [agent-pair lhs-rhs]
           (let [[[lhs-id {name :name, lstates :states, lbindings :bindings} :as lhs-agent]
                  [rhs-id {rstates :states, rbindings :bindings}]] agent-pair] ; rname = lname
             (for [site (set/union (-> lstates keys set)
                                   (-> rstates keys set))]
               (let [ls (lstates site), lb (lbindings site),
                     rs (rstates site), rb (rbindings site)]
                 (vector (cond
                           (some #{[lhs-id site]} @done-sites) nil
                           
                           (xor (= lb :semi-link) (= rb :semi-link))
                           (throw (Exception. "Semi-links must be balanced in lhs and rhs."))
                           
                           (and (not (number? lb)) (number? rb)) ; bind
                           ;; nb-site is the site at the other end of the link
                           (let [nb-site (key (first (filter #(= (val %) rhs-id)
                                                             (-> rb rhs :bindings))))
                                 ;; lhs-nb is the equivalent to rb
                                 lhs-nb (ffirst (filter (comp #{rb} second) lhs-rhs))]
                             ;; register the bound agent in done-sites
                             (swap! done-sites #(conj % [lhs-nb nb-site]))
                             ;; bind args: [c1 a1 s1 c2 a2 s2]
                             (bind-agents (complex lhs-agent lhs) lhs-id site
                                          (complex lhs-nb lhs) lhs-nb nb-site))
                           
                           (and (number? lb) (not (number? rb))) ; unbind
                           (let [nb-site (key (first (filter #(= (val %) lhs-id)
                                                             (-> lb lhs :bindings))))]
                             ;; register the unbound agent in done-sites
                             (swap! done-sites #(conj % [lb nb-site]))
                             ;; unbind args: [c1 a1 s1 c2 a2 s2]
                             (unbind-agents (complex lhs-agent lhs) lhs-id site
                                            (complex lb lhs) lb nb-site))
                           
                           :else nil)
                         
                         (cond
                           (or (= ls rs) (nil? rs)) nil ; rs is nil if it's not written in rhs
                           ;; create args: [c a s new-state]
                           :else (modify-state (complex lhs-agent lhs) lhs-id site rs))))))))
       (apply concat)
       (apply concat)
       (apply concat)))

(defn elementary-actions [lhs rhs]
  (let [[lhs-rhs created-agents removed-agents] (pairing-lhs-rhs lhs rhs)
        modified-sites (get-modified-sites lhs-rhs)
        
        [modify-state-fns bind-agents-fns unbind-agents-fns]
        ((juxt get-modify-state-fns get-bind-agents-fns get-unbind-agents-fns) modified-sites lhs)]
    
    {:elementary-actions (concat (map create created-agents)
                                 (map #(destroy (complex % lhs) %) removed-agents)
                                 modify-state-fns bind-agents-fns unbind-agents-fns)
     :modified-sites modified-sites}))

(defn action [lhs rhs]
  (with-meta
    (fn [chamber matching]
      (let [{keys [:elementary-actions :modified-sites]} (elementary-actions lhs rhs)]
        ((apply comp (map #(partial % matching) elementary-actions)) chamber)))
    {:modified-sites modified-sites}))

;; TODO
(defn count-automorphisms [expr] 1)

(defn create-rule [name lhs rhs rate]
  (Rule. name (make-pattern (with-meta lhs {:automorphisms (count-automorphisms lhs)}))
         (make-pattern rhs) rate (action lhs rhs)))

(defn rule?
  "Check if obj is a Kappa rule."
  [obj]
  (instance? Rule obj))


;; TODO reachable-complexes and reachable-reactions
(defn reachable-complexes
  "Returns a lazy seq of all the complexes reachable by the system."
  [rule-set initial-state]
  nil)

(defn reachable-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system."
  [rule-set initial-state]
  nil)

