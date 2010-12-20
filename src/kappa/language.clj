(ns kappa.language
  {:doc "Core data-structures and functions for the Kappa language."
   :author "Ricardo Honorato-Zimmer"}
  (:require [kappa.misc :as misc]
            [clojure.contrib.combinatorics :as comb]
            [clojure.set :as set]))

;;; TODO an Agent record is an "agent specification".
;;;      a pair [id agent-spec] is an agent
;;;      a map {id agent-spec} is a one-agent expression
;;; Esta nomenclatura deberia simplificar la descripcion de las funciones
;;; Ademas, estoy seguro que por ahi hay algunas funciones que reciben un agente,
;;; cuando deberian recibir solo un id, como por ejemplo complex.

;;; Agents
(defrecord Agent [name states bindings])
;; states is a map from site names (as keywords)
;; to internal site states (as strings)
;; bindings is a map from site names to one
;; of the tags: :free, :unspecified, :semi-link
;; or the id of the bound agent
;; ids are assigned by the expression

(defn make-agent [name states bindings]
  (Agent. name states bindings))

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

(defn get-neighbours
  "Get the subexpression containing the neighbours of a."
  [expr [id a]]
  (for [nb-id (->> a :bindings vals (filter #(number? %)))]
    [nb-id (expr nb-id)]))

(defn complex
  "Returns the ids of the complex to which initial-oae belongs."
  [expr initial-oae]
  (set (map first
            (misc/pre-traverse (partial get-neighbours expr) initial-oae))))

(defn compute-complexes [expr]
  (let [get-complex (partial complex expr)
        step (fn step [remaining]
               (if (empty? remaining)
                 nil
                 (let [c (get-complex (first remaining))]
                   (cons c (lazy-seq
                             (step (doall (remove (comp c key) (rest remaining)))))))))]
    (step expr)))

(defn with-complexes [expr]
  (vary-meta expr merge {:complexes (compute-complexes expr)}))

(defn subexpr
  "Gets the subexpression for the given ids and expression expr."
  [expr ids]
  (zipmap ids (map expr ids)))

(defn mix-exprs [& exprs]
  (with-meta (reduce into {} exprs)
    {:complexes (mapcat (comp :complexes meta) exprs)}))

;; print-method can't tell if its argument is an expression or other map, so...
(defn expr-str [expr]
  (apply str (interpose ", " (map print-str (vals expr)))))

(defn expression?
  "Check if obj is a Kappa expression."
  [obj]
  (and (map? obj)
       (every? number? (keys obj))
       (every? agent? (vals obj))))


;;; Match
(defn match-agent
  "Check if agent p matches agent a."
  [p a] ; p stands for pattern, a for agent
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
                    :free (= ((:bindings a) %) :free)
                    (not (= ((:bindings a) %) :free)))
                 (keys p-bindings)))))

(defn match-expr
  "Check if the given pattern p matches the given expression a."
  [p e] ; p stands for pattern, e for expression
  ;; stores all matchings for every agent in p (lazyly :)
  (let [matchings (map (fn [p] (filter #(match-agent p (val %)) e)) (vals p))
        match-comb (apply comb/cartesian-product matchings) ; matching combinations
        n (count p)]
    ;; try every combination of matchings checking if the neighbours corresponds
    (loop [left match-comb]
      (if (empty? left)
        nil
        (let [pa->ea (zipmap (vals p) (vals (first left)))
              pa-id->ea-id (zipmap (keys p) (keys (first left)))]
          ;; the neighbours of each agent in the pattern must match
          ;; the neighbours of each agent in the expression
          (if (and (= (count pa->ea) n)
                   (every? (fn [[pa ea]] ; iterates over every matching pa => ea
                             (every? true? ; iterates over every binding of pa
                                     (map (fn [[site binding]]
                                            (case binding
                                              :unspecified true
                                              :semi-link (not (= ((:bindings ea) site) :free))
                                              :free (= ((:bindings ea) site) :free)
                                              (= ((:bindings ea) site)
                                                 (pa-id->ea-id binding))))
                                          (:bindings pa))))
                           pa->ea))
            pa-id->ea-id
            (recur (rest left))))))))

(defn domain2codomain
  "Returns a map from pairs [agent site] in p-complexes to pairs [agent site]
  in e-complexes that matches. e- and p-complexes must be seqs of expressions,
  each expression representing just one complex.

  The keys of the returning map are the domain and the values the codomain."
  [expr p-complexes e-complexes]
  (into {} (for [p-complex p-complexes
                 matched-complex (keep (partial match-expr p-complex) e-complexes)
                 [pa-id pa] p-complex
                 pa-site (-> pa :states keys)
                 :let [ea-id (matched-complex pa-id)]]
             [[pa-id pa-site] [ea-id pa-site]])))


;;; Rules
(defrecord Rule [name lhs rhs rate action])
;; rate is the deterministic kinetic constant

(defmethod print-method Rule [r w]
  (.write w (str \{ (:lhs r) " -> " (:rhs r) " @ " (:rate r) \})))

(defn modify-state
  "Returns a function that modifies the state of site s in agent
  ((matching c) a) to new-state."
  [c [a-id _] s new-state] ; c is a seq of lhs agent ids. a is a lhs agent id.
  ;; matching is a map from lhs complex to a map from lhs agents ids to mixture agents ids
  (fn [chamber matching]
    (let [ma-id ((matching c) a-id)]
      (-> chamber
        (assoc-in [:mixture ma-id :states s] new-state)
        (vary-meta update-in [:modified-sites] conj [ma-id s])))))

(defn bind-agents
  "Returns a function that binds agents a1 and a2 through
  sites s1 and s2, respectively."
  [c1 [a1-id _] s1 c2 [a2-id _] s2]
  (fn [chamber matching]
    (let [ma1 ((matching c1) a1-id)
          ma2 ((matching c2) a2-id)]
      (-> chamber
        (assoc-in [:mixture ma1 :bindings s1] ma2)
        (assoc-in [:mixture ma2 :bindings s2] ma1)
        (vary-meta update-in [:modified-sites] into [[ma1 s1] [ma2 s2]])))))

(defn unbind-agents
  "Returns a function that unbinds agents a1 and a2."
  ([c1 [a1-id _] s1 c2 [a2-id _] s2]
     (fn [chamber matching]
       (let [ma1 ((matching c1) a1-id)
             ma2 ((matching c2) a2-id)]
         ;;(when (or (not (contains? (:mixture chamber) ma1))
         ;;          (not (contains? (:mixture chamber) ma2)))
         ;;  (swank.core/break))
         (-> chamber
           (assoc-in [:mixture ma1 :bindings s1] :free)
           (assoc-in [:mixture ma2 :bindings s2] :free)
           (vary-meta update-in [:modified-sites] into [[ma1 s1] [ma2 s2]])))))
  ([c1 [a1-id _] s1]
     (fn [chamber matching]
       (let [ma1 ((matching c1) a1-id)]
         (-> chamber
           (assoc-in [:mixture ma1 :bindings s1] :free)
           (vary-meta update-in [:modified-sites] into [[ma1 s1]]))))))

(defn create
  "Returns a function that creates an agent a in chamber's mixture.
  Agent a must not be bound."
  [agent-spec]
  (if (some #(or (number? %) (= % :semi-link)) (vals (:bindings agent-spec)))
    (throw (Exception. "cannot create bound agents"))
    (fn [chamber]
      (let [id (misc/counter)]
        (-> chamber
          (assoc-in [:mixture id] agent-spec)
          (vary-meta update-in [:added-agents] conj id))))))

(defn create-agent
  "Returns a function that creates an agent a in chamber's mixture.
  Agent a must not be bound."
  [a]
  (let [f (create a)]
    (fn [chamber _]
      (f chamber))))

(defn destroy [id]
  (fn [chamber]
    (let [mixture (:mixture chamber)
          nb-ids (filter number? (vals (:bindings (mixture id))))]
      (reduce (fn [chamber nb-id]
                (let [nb-site (first (filter (comp #{id} val) (:bindings (mixture nb-id))))]
                  (-> chamber
                    (assoc-in [:mixture nb-id :bindings nb-site] :free)
                    (vary-meta update-in [:modified-sites] conj [nb-id nb-site]))))
              (-> chamber
                (update-in [:mixture] dissoc id)
                (vary-meta update-in [:removed-agents] conj id))
              nb-ids))))

(defn destroy-agent [c [a-id _]]
  (fn [chamber matching]
    (let [ma ((matching c) a-id)]
      ((destroy ma) chamber))))

(defn pair-exprs
  "Returns a seq of three things:
  1. A seq of pairs [lhs oae, rhs oae] which will be considered to be equivalent.
  2. A seq of rhs oaes that doesn't have a counterpart in lhs. These agents will be
     created by the rule.
  3. A seq of lhs oaes that doesn't have a counterpart in rhs. These agents will be
     destroyed by the rule."
  [lhs rhs]
  (cond
    (empty? lhs) [nil (seq rhs) nil]
    (empty? rhs) [nil nil (seq lhs)]

    :else
    (let [[lhs-names rhs-names] (map #(set (map (comp :name val) %)) [lhs rhs])
          
          [lhs-agents-by-name rhs-agents-by-name]
          (map (fn [expr names]
                 (apply merge (for [name names]
                                {name (filter #(= name (-> % val :name))
                                              (sort-by key expr))})))
               [lhs rhs] [lhs-names rhs-names])]
      
      (->> (for [name (set/union lhs-names rhs-names)]
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
        (map (partial apply concat))))))

(defn get-lhs-agent [id lhs-rhs]
  (ffirst (filter (comp #{id} key second) lhs-rhs)))

(defn get-rhs-agent [id lhs-rhs]
  (second (first (filter (comp #{id} key first) lhs-rhs))))

(defn get-modified-sites [lhs-rhs lhs]
  (->> (for [agent-pair lhs-rhs]
         (let [[[lhs-id {lstates :states, lbindings :bindings} :as lhs-agent]
                [rhs-id {rstates :states, rbindings :bindings}]] agent-pair]
           (for [site (set/union (-> lstates keys set)
                                 (-> rstates keys set))]
             (let [ls (lstates site), lb (lbindings site),
                   rs (rstates site), rb (rbindings site)]
               ;;(swank.core/break)
               (merge
                (when-not (or (= ls rs) (nil? rs))
                  {:modified [[lhs-agent site rs]]})
                (when (and (not (number? lb)) (number? rb))
                  {:bound [[(get-lhs-agent rb lhs-rhs) lhs-agent site]]})
                (when (and (number? lb) (number? rb) (not= lb (get-lhs-agent rb lhs-rhs)))
                  {:unbound [[(find lhs lb) lhs-agent site]] ;; when it unbinds and then
                   :bound [[(get-lhs-agent rb lhs-rhs) lhs-agent site]]}) ;; binds to other agent
                (when (and (number? lb) (not (number? rb)))
                  {:unbound [[(find lhs lb) lhs-agent site]]})
                (when (and (= lb :semi-link) (not= rb :semi-link))
                  {:unbound [[nil lhs-agent site]]}))))))
       (apply concat)
       (apply merge-with concat)))

(defn- get-modify-state-fns [{mss :modified} lhs]
  (map (fn [[lhs-agent site new-state]]
         (modify-state (complex lhs lhs-agent) lhs-agent site new-state))
       mss))

(defn- get-fns [f ss lhs]
  (for [site-pair (into #{} (for [[a2 a1 s1] ss
                                  :let [s2 (->> ss
                                             (filter (comp #{[a1 a2]} #(take 2 %)))
                                             first
                                             misc/third)]]
                              #{[a1 s1] [a2 s2]}))]
    ;; sets doesn't preserve order, so these [a1 s1] and [a2 s2] are not the same as above
    (let [[a1 s1] (first site-pair), [a2 s2] (second site-pair)]
      ;;(swank.core/break)
      (cond
        (some nil? [a2 s2]) (f (complex lhs a1) a1 s1)
        (some nil? [a1 s1]) (f (complex lhs a2) a2 s2)
        :else (f (complex lhs a1) a1 s1 (complex lhs a2) a2 s2)))))

(defn- get-bind-agents-fns [{bss :bound} lhs]
  (get-fns bind-agents bss lhs))

(defn- get-unbind-agents-fns [{uss :unbound} lhs]
  (get-fns unbind-agents uss lhs))

(defn elementary-actions [lhs rhs]
  (let [[lhs-rhs created-agents removed-agents] (pair-exprs lhs rhs)
        modified-sites (get-modified-sites lhs-rhs lhs)

        [modify-state-fns bind-agents-fns unbind-agents-fns]
        ((juxt get-modify-state-fns get-bind-agents-fns get-unbind-agents-fns) modified-sites lhs)

        ;; get every modified site
        {:keys [bound unbound modified]} modified-sites

        [bss-lhs uss-lhs] (map #(for [[_ a1 s1] %] [a1 s1]) [bound unbound])
        [bss-rhs uss-rhs] (map #(for [[[a1-id _] s1] %]
                                  [(get-rhs-agent a1-id lhs-rhs) s1])
                               [bss-lhs uss-lhs])
        
        mss (apply concat
                   (for [[lhs-agent site _] modified] ; get every modified site! in lhs and rhs!
                     [[lhs-agent site] [(get-rhs-agent (key lhs-agent) lhs-rhs) site]]))

        [css rss] (map #(for [[id {states :states} :as oae] %, site (keys states)]
                          [oae site])
                       [created-agents removed-agents])]
    
    {:elementary-actions (concat modify-state-fns bind-agents-fns unbind-agents-fns
                                 (map create-agent (map val created-agents))
                                 (map #(destroy-agent (complex lhs %) %) removed-agents))
     :modified-sites (concat mss bss-lhs bss-rhs uss-lhs uss-rhs css rss)}))

(defn action [lhs rhs]
  (let [{eas :elementary-actions, mss :modified-sites} (elementary-actions lhs rhs)]
    ;;(dorun eas) ;; DEBUG
    (with-meta
      (fn [chamber matching]
        ((apply comp (map #(fn [chamber] (% chamber matching)) eas)) chamber))
      {:modified-sites (doall mss)})))

;; TODO: isomorphism fn... meanwhile:
(defn count-automorphisms [expr]
  (misc/factorial 
   (reduce + 1
           (for [[c1 c2] (comb/combinations (map (partial subexpr expr)
                                                 (-> expr meta :complexes)) 2)]
             (if (match-expr c1 c2) 1 0)))))

(defn make-rule [name lhs rhs rate]
  (Rule. name (vary-meta lhs merge
                {:automorphisms (count-automorphisms lhs)})
         rhs rate (action lhs rhs)))

(defn rule?
  "Check if obj is a Kappa rule."
  [obj]
  (instance? Rule obj))

