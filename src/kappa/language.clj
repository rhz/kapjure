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


;;;; Agents

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

(let [cnt (atom 0)] ; this is real encapsulation ;)
  (defn get-unique-agent-id [] (swap! cnt inc))
  (defn reset-agent-id-counter [] (reset! cnt 0)))

(defn agent?
  "Check if obj is a Kappa agent."
  [obj]
  (instance? Agent obj))



;;;; Expressions

;; an expression is a map from ids to Agents

(defn subexpr
  "Gets the subexpression for the given ids and expression expr."
  [expr ids]
  (zipmap ids (map expr ids)))

(defn find-partner
  [expr a-id site]
  (let [partner-id (get-in expr [a-id :bindings site])
        ;; FIXME this will fail for agents that are bound by two links
        partner-site (first (filter (comp #{a-id} val) (:bindings (expr partner-id))))]
    [partner-id partner-site]))

(defn complex
  "Returns the ids of the complex to which a-id belongs."
  [expr a-id]
  (set (misc/pre-traverse #(filter number? (vals (:bindings (expr %)))) a-id)))

(defn complexes [expr]
  (map (partial subexpr expr) (-> expr meta :complexes)))

(defn compute-complexes [expr]
  (let [get-complex (partial complex expr)
        step (fn step [remaining]
               (if (empty? remaining)
                 nil
                 (let [c (get-complex (key (first remaining)))]
                   (cons c (lazy-seq
                             (step (doall (remove (comp c key) (rest remaining)))))))))]
    (step expr)))

(defn with-complexes [expr]
  (vary-meta expr merge {:complexes (compute-complexes expr)}))

(defn mix-exprs [& exprs]
  (with-meta (reduce into {} exprs)
    {:complexes (mapcat (comp :complexes meta) exprs)}))

;; print-method can't tell if its argument is an expression or other kind of map, so...
(defn expr-str [expr]
  (apply str (interpose ", " (map print-str (vals expr)))))

(defn expression?
  "Check if obj is a Kappa expression."
  [obj]
  (and (map? obj)
       (every? number? (keys obj))
       (every? agent? (vals obj))))



;;;; Match

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
          (if (and (= (count pa->ea) n)
                   ;; neighbours must match
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



;;;; Rules

(defrecord Rule [name lhs rhs rate action])

(defmethod print-method Rule [r w]
  (.write w (str \{ (:lhs r) " -> " (:rhs r) " @ " (:rate r) \})))


;;; Actions

(defn modify-state-in-expr
  [expr id s val]
  (assoc-in expr [id :states s] val))

(defn modify-state-in-chamber
  [chamber id s val]
  (-> chamber
      (assoc-in [:mixture id :states s] val)
      (vary-meta update-in [:modified-sites] conj [id s])))

(defn modify-state-action
  "Returns a fn that modifies the state of site s in agent ((matching c) a-id) to val."
  [c a-id s val]
  ;; matching is a map from lhs complex to a map from lhs agents ids to mixture agents ids
  (fn [chamber matching]
    (modify-state-in-chamber chamber ((matching c) a-id) s val)))


(defn bind-agents-in-expr
  [expr a1-id s1 a2-id s2]
  (-> expr
      (assoc-in [a1-id :bindings s1] a2-id)
      (assoc-in [a2-id :bindings s2] a1-id)))

(defn bind-agents-in-chamber
  [chamber a1-id s1 a2-id s2]
  (-> chamber
      (update-in [:mixture] bind-agents-in-expr a1-id s1 a2-id s2)
      (vary-meta update-in [:modified-sites] into [[a1-id s1] [a2-id s2]])))

(defn bind-agents-action
  "Returns a fn that binds agents a1 and a2 through sites s1 and s2, respectively."
  [c1 a1-id s1 c2 a2-id s2]
  (fn [chamber matching]
    (let [ma1 ((matching c1) a1-id)
          ma2 ((matching c2) a2-id)]
      (bind-agents-in-chamber chamber ma1 s1 ma2 s2))))


(defn unbind-agents-in-expr
  ([expr a1-id s1 a2-id s2]
     (-> expr
         (assoc-in [a1-id :bindings s1] :free)
         (assoc-in [a2-id :bindings s2] :free)))
  ([expr a1-id s1]
     (let [[a2-id s2] (find-partner expr a1-id s1)]
       (unbind-agents-in-expr expr a1-id s1 a2-id s2))))

(defn unbind-agents-in-chamber
  ([chamber a1-id s1 a2-id s2]
     (-> chamber
         (update-in [:mixture] unbind-agents-in-expr a1-id s1 a2-id s2)
         (vary-meta update-in [:modified-sites] into [[a1-id s1] [a2-id s2]])))
  ([chamber a1-id s1]
     (let [[a2-id s2] (find-partner (:mixture chamber) a1-id s1)]
       (unbind-agents-in-chamber chamber a1-id s1 a2-id s2))))

(defn unbind-agents-action
  "Returns a function that unbinds agents a1 and a2."
  ([c1 a1-id s1 c2 a2-id s2]
     (fn [chamber matching]
       (let [ma1 ((matching c1) a1-id)
             ma2 ((matching c2) a2-id)]
         (unbind-agents-in-chamber chamber ma1 s1 ma2 s2))))
  ([c1 a1-id s1]
     (fn [chamber matching]
       (let [ma1 ((matching c1) a1-id)]
         (unbind-agents-in-chamber chamber ma1 s1)))))


(defn create-agent-in-expr
  [expr agent-spec]
  (let [id (get-unique-agent-id)]
    (assoc expr id agent-spec)))

(defn create-agent-in-chamber
  "Creates an agent in chamber's mixture. The given agent should not be bound."
  [chamber agent-spec]
  (let [id (get-unique-agent-id)]
    (-> (assoc-in chamber [:mixture id] agent-spec)
        (vary-meta update-in [:added-agents] conj id))))

(defn create-agent-action
  "Returns a fn that creates an agent in chamber's mixture.
  The returned fn receives two arguments, the first one beign the chamber and
  the second argument is the matching (a map from agent ids to mixture ids)."
  [agent lhs-rhs]
  (fn [chamber matching]
    ;; check if agent is bound
    (if (every? #(and (= % :free) (= % :unspecified)) (-> agent val :bindings vals))
      (create-agent-in-chamber chamber (val agent))
      ;; bounded agent
      ;; TODO is there a simpler way to do this?
      (let [a-id (get-unique-agent-id)
            rhs-lhs (into {} (map (comp vec reverse) lhs-rhs))
            site+nb-rule-ids (filter (comp number? val) (-> agent val :bindings))
            ;; FIXME this will not work if bounded agent is created by the rule
            site+nb (for [[s nb-id] site+nb-rule-ids
                          :let [nb (first (filter (comp #{nb-id} key)
                                                  (keys rhs-lhs)))
                                lhs-id (key (rhs-lhs nb))
                                c (first (filter (partial some #{lhs-id}) (keys matching)))
                                nb-site (key (first (filter (comp #{(key agent)} val)
                                                            (:bindings (val nb)))))]]
                      [s ((matching c) lhs-id) nb-site])]
        (reduce (fn [chamber [s nb-id nb-site]]
                  ;; is it necessary to annotate these bindings in chamber's metadata?
                  (bind-agents-in-chamber chamber a-id s nb-id nb-site))
                (-> (assoc-in chamber [:mixture a-id] (val agent))
                    (vary-meta update-in [:added-agents] conj a-id))
                site+nb)))))


(defn destroy-agent-in-expr
  [expr id]
  (let [nb-ids (filter number? (vals (:bindings (expr id))))]
    (reduce (fn [expr nb-id]
              (let [nb-site (first (filter (comp #{id} val)
                                           (:bindings (expr nb-id))))]
                (assoc-in expr [nb-id :bindings nb-site] :free)))
            (dissoc expr id)
            nb-ids)))

(defn destroy-agent-in-chamber
  [chamber id]
  (let [mixture (:mixture chamber)
        nb-ids (filter number? (vals (:bindings (mixture id))))]
    (reduce (fn [chamber nb-id]
              (if-let [nb (mixture nb-id)]
                (let [nb-site (first (filter (comp #{id} val) (:bindings nb-id)))]
                  (-> chamber
                      (assoc-in [:mixture nb-id :bindings nb-site] :free)
                      (vary-meta update-in [:modified-sites] conj [nb-id nb-site])))
                chamber))
            (-> chamber
                (update-in [:mixture] dissoc id)
                (vary-meta update-in [:removed-agents] conj id))
            nb-ids)))

(defn destroy-agent-action
  [c a-id]
  (fn [chamber matching]
    (destroy-agent-in-chamber chamber ((matching c) a-id))))


;;; Action inference

(defn- pair-agents-by-name
  [lhs rhs]
  (let [[lhs-names rhs-names] (map #(set (map (comp :name val) %)) [lhs rhs])]
    (map (fn [expr names]
           (into {} (for [name names]
                      [name (filter (comp #{name} :name val) (sort expr))])))
         [lhs rhs] [lhs-names rhs-names])))

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
    (let [[lhs-agents-by-name rhs-agents-by-name] (pair-agents-by-name lhs rhs)]
      (->> (for [name (set (concat (keys lhs-agents-by-name) (keys rhs-agents-by-name)))]
             (let [lhs-agents (lhs-agents-by-name name)
                   rhs-agents (rhs-agents-by-name name)
                   n (count lhs-agents)
                   m (count rhs-agents)]
               (cond
                 (= n m) [(map vector lhs-agents rhs-agents) nil nil]

                 (< n m) (let [[rhs-agents created-agents] (split-at (count lhs-agents) rhs-agents)]
                           [(map vector lhs-agents rhs-agents) created-agents nil])

                 (> n m) (let [[lhs-agents removed-agents] (split-at (count rhs-agents) lhs-agents)]
                           [(map vector lhs-agents rhs-agents) nil removed-agents]))))
           (apply map vector)
           (map (partial apply concat))))))

(defn get-lhs-id [rhs-id lhs-rhs]
  (if-let [[[[lhs-id _] _]] (filter (comp #{rhs-id} key second) lhs-rhs)]
    lhs-id
    nil))

(defn get-rhs-id [lhs-id lhs-rhs]
  (if-let [[[_ [rhs-id _]]] (filter (comp #{lhs-id} key first) lhs-rhs)]
    rhs-id
    nil))

(defn get-modified-sites [lhs-rhs lhs]
  (->> (for [agent-pair lhs-rhs]
         (let [[[lhs-id {lstates :states, lbindings :bindings} :as lhs-agent]
                [rhs-id {rstates :states, rbindings :bindings}]] agent-pair]
           (for [site (set/union (-> lstates keys set)
                                 (-> rstates keys set))]
             (let [ls (lstates site), lb (lbindings site)
                   rs (rstates site), rb (rbindings site)]
               (merge
                (when-not (or (= ls rs) (nil? rs))
                  {:modified [[lhs-id site rs]]})
                (when (and (not (number? lb)) (number? rb))
                  {:bound [[(get-lhs-id rb lhs-rhs) lhs-id site]]})
                (when (and (number? lb) (not (number? rb)))
                  {:unbound [[lb lhs-id site]]})
                (when (and (= lb :semi-link) (not= rb :semi-link))
                  {:unbound [[nil lhs-agent site]]})
                (when (and (number? lb) (number? rb))
                  (if-let [a2-id (get-lhs-id rb lhs-rhs)]
                    (when (not= lb a2-id)
                      {:unbound [[lb lhs-id site]]
                       :bound [[a2-id lhs-id site]]})
                    {:unbound [[lb lhs-id site]]})))))))
       (apply concat)
       (apply merge-with concat)))

(defn- get-modify-state-fns [f mss lhs]
  (for [[lhs-id site new-state] mss]
    (f (complex lhs lhs-id) lhs-id site new-state)))

(defn- get-bind-unbind-fns [f mss lhs]
  (for [site-pair (into #{} (for [[a2-id a1-id s1] mss
                                  :let [[[_ _ s2]] (filter #(= [a1-id a2-id] (take 2 %)) mss)]]
                              #{[a1-id s1] [a2-id s2]}))]
    ;; sets doesn't preserve order, so these [a1 s1] and [a2 s2] are not the same as above
    (let [[a1-id s1] (first site-pair), [a2-id s2] (second site-pair)]
      (cond
        (some nil? [a2-id s2]) (f (complex lhs a1-id) a1-id s1)
        (some nil? [a1-id s1]) (f (complex lhs a2-id) a2-id s2)
        :else (f (complex lhs a1-id) a1-id s1 (complex lhs a2-id) a2-id s2)))))

(defn elementary-actions [lhs rhs]
  (let [[lhs-rhs created-agents removed-agents] (pair-exprs lhs rhs)
        {:keys [bound unbound modified]} (get-modified-sites lhs-rhs lhs)

        modify-state-fns  (get-modify-state-fns modify-state-action  modified lhs)
        bind-agents-fns   (get-bind-unbind-fns  bind-agents-action   bound    lhs)
        unbind-agents-fns (get-bind-unbind-fns  unbind-agents-action unbound  lhs)

        ;; get every modified site! in lhs and rhs!
        [bss-lhs uss-lhs] (map #(map (partial drop 1) %) [bound unbound])
        [bss-rhs uss-rhs] (map #(for [[lhs-id site] %]
                                  [(get-rhs-id lhs-id lhs-rhs) site])
                               [bss-lhs uss-lhs])
        mss (mapcat (fn [[lhs-id site _]]
                      [[lhs-id site] [(get-rhs-id lhs-id lhs-rhs) site]])
                    modified)
        [css rss] (map #(for [[id {states :states}] %
                              site (keys states)]
                          [id site])
                       [created-agents removed-agents])]

    {:elementary-actions (concat (map #(create-agent-action % lhs-rhs)
                                      created-agents)
                                 (map #(destroy-agent-action (complex lhs %) %)
                                      (map key removed-agents))
                                 modify-state-fns bind-agents-fns unbind-agents-fns)
     :modified-sites (concat mss bss-lhs bss-rhs uss-lhs uss-rhs css rss)}))

(defn action [lhs rhs]
  (let [{eas :elementary-actions, mss :modified-sites} (elementary-actions lhs rhs)]
    ;;(dorun eas) ;; DEBUG
    (with-meta
      (fn [chamber matching]
        ((apply comp (map #(fn [chamber] (% chamber matching)) eas)) chamber))
      {:modified-sites (doall mss)})))


;;; Isomorphisms
;; TODO isomorphism fn... meanwhile:
(defn count-automorphisms [expr]
  (misc/factorial 
   (reduce + 1
           (for [[c1 c2] (comb/combinations (map (partial subexpr expr)
                                                 (-> expr meta :complexes)) 2)]
             (if (match-expr c1 c2) 1 0)))))

;;; Ctor
(defn make-rule
  [name lhs rhs rate]
  (Rule. name (vary-meta lhs merge
                {:automorphisms (count-automorphisms lhs)})
         rhs rate (action lhs rhs)))

;;; Predicate
(defn rule?
  "Check if obj is a Kappa rule."
  [obj]
  (instance? Rule obj))

