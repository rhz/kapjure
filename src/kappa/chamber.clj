(ns kappa.chamber
  {:doc "Chambers are conceptual compartments where reactions take place."
   :author "Ricardo Honorato-Zimmer"}
  (:require [kappa.maps :as maps]
            [kappa.misc :as misc]
            [kappa.language :as lang]
            [clojure.set :as set]
            [clojure.contrib.math :as m]
            [clojure.contrib.generic.math-functions :as math]))


(defrecord DeterministicRateChamber [rules mixture time event-cnt clash-cnt volume stochastic-cs
                                     activity-map matching-map lift-map ram rim obs-exprs obs-rules])

(defrecord StochasticRateChamber [rules mixture time event-cnt clash-cnt
                                  activity-map matching-map lift-map ram rim obs-exprs obs-rules])

;;; Stochastic constants
(defn k2c
  "Computes the stochastic rate constant for a rule, assuming its rate is deterministic
  and it is expressed in [mol^(n-1) * (dm^-3)^(n-1) * s^-1], where n is the number of complexes
  in the left-hand-side of the rule (i.e., the reaction's order). The volume of the chamber
  must also be is expressed in dm^3 (or the same space units in rule's rate).

  Please note that the use of deterministic-rate chambers is experimental and has not been
  tested thoroughly."
  [volume rule]
  (let [N-avogadro 6.022E23
        rate (:rate rule)
        num-complexes (-> rule :lhs meta :complexes count)
        aut (-> rule :lhs meta :automorphisms)]
    
    (cond
      (zero? num-complexes) (/ (* rate volume) N-avogadro)
      (= num-complexes 1) rate
      (> num-complexes 1) (/ (* rate aut (m/expt N-avogadro (dec num-complexes)))
                             (m/expt volume (dec num-complexes))))))

(defn- update-stochastic-cs [chamber]
  (assoc chamber :stochastic-cs (zipmap (:rules chamber)
                                        (map (partial k2c (:volume chamber)) (:rules chamber)))))

(defn update-volume-and-cs
  ""
  [chamber volume]
  (-> chamber
    (assoc :volume volume)
    (update-stochastic-cs)))

;;; Activities
(defn- activity [ms k aut] ; matchings (ms) and stochastic kinetic constant (k)
  (/ (apply * k (map (comp count val) ms)) aut))

(defmulti update-activities
  "Update the activities of rules, if given. Otherwise update all activities."
  {:arglists '([chamber] [chamber rules])}
  (fn [chamber & _] (class chamber)))

(defmethod update-activities DeterministicRateChamber
  ([chamber]
     (let [mm (:matching-map chamber)
           cs (:stochastic-cs chamber)]
       (assoc chamber :activity-map
              (into {} (for [r (:rules chamber)]
                         [r (activity (mm r) (cs r) (-> r :lhs meta :automorphisms))])))))
  ([chamber rules]
     (let [mm (:matching-map chamber)
           cs (:stochastic-cs chamber)]
       (update-in chamber [:activity-map]
                  #(reduce (fn [am r]
                             (assoc am r (activity (mm r) (cs r)
                                                   (-> r :lhs meta :automorphisms))))
                           % rules)))))

(defmethod update-activities StochasticRateChamber
  ([chamber]
     (let [mm (:matching-map chamber)]
       (assoc chamber :activity-map
              (into {} (for [r (:rules chamber)]
                         [r (activity (mm r) (:rate r) (-> r :lhs meta :automorphisms))])))))
  ([chamber rules]
     (let [mm (:matching-map chamber)]
       (update-in chamber [:activity-map]
                  #(reduce (fn [am r]
                             (assoc am r (activity (mm r) (:rate r)
                                                   (-> r :lhs meta :automorphisms))))
                           % rules)))))

;;; Constructors
(defn make-chamber
  "Chambers are conceptual compartments where reactions take place.
  There are two types of chamber: one considers rule rates as deterministic
  and the other as stochastic. The former is selected by :rate-type :deterministic
  and the latter by :rate-type :stochastic.

  Deterministic-rate chambers take the rule's rate as a deterministic one and transform it
  to its stochastic counterpart using the logic described in kappa.chamber/k2c's source code.

  Please note that the use of deterministic-rate chambers is experimental and has not been
  tested thoroughly."
  [rules mixture obs-exprs obs-rules & {:keys [rate-type volume] :or {rate-type :stochastic}}]
  (let [[mm lf] (maps/matching-and-lift-map rules mixture)]
    (case rate-type
      :deterministic
      (-> (DeterministicRateChamber. rules mixture 0 0 0 volume
                                     [] [] ; stochastic cs and activities are computed later
                                     mm lf (maps/activation-map rules) (maps/inhibition-map rules)
                                     (maps/obs-map obs-exprs mixture) ; observed expressions
                                     (zipmap obs-rules (repeat {}))) ; observed rules
        (update-stochastic-cs)
        (update-activities))
      
      :stochastic
      (-> (StochasticRateChamber. rules mixture 0 0 0 [] ; activities are computed later
                                  mm lf (maps/activation-map rules) (maps/inhibition-map rules)
                                  (maps/obs-map obs-exprs mixture) ; observed expressions
                                  (zipmap obs-rules (repeat {}))) ; observed rules
        (update-activities)))))

;;; Events
(defn- time-advance [activities]
  (/ (math/log (/ 1 (rand)))
     (apply + (vals activities))))

(defn- select-rule [activities]
  (misc/choice activities))

(defn- select-matching-per-complex [mm]
  (apply merge (for [[c m] mm]
                 {c (rand-nth m)})))

(defn- clash? [m]
  (let [agent-ids (for [[c matchings] m
                        [lhs-agent mixture-agent] matchings]
                    mixture-agent)]
    (not (every? #(= (val %) 1) (frequencies agent-ids)))))

(defn- negative-update [chamber]
  (let [lift-map (:lift-map chamber)
        m (meta chamber)
        ras (:removed-agents m)
        mss (:modified-sites m)
        r-c-cods (apply concat (concat
                                (for [[ma ms] mss]
                                  ((lift-map ma) ms))
                                (for [ra ras]
                                  (apply concat (vals (lift-map ra))))))
        cod-mates (for [{r :rule, c :complex, cod :codomain} r-c-cods
                        [a-id s] cod]
                    [a-id s r c cod])]
    (-> chamber
        ;; remove phi_c from Phi(r', c)
        (update-in [:matching-map]
                   #(reduce (fn remove-phi [mm {r :rule, c :complex, cod :codomain}]
                              (let [matching (zipmap c (map first cod))]
                                (update-in mm [r c] disj matching)))
                            % r-c-cods))
        ;; for all pairs (b, y) in cod(phi_c) remove <r', c, phi_c> from l(b, y)
        (update-in [:lift-map]
                   #(reduce (fn lm-update [lm [a-id s r c cod]]
                              (update-in lm [a-id s]
                                         (fn [lm-vals]
                                           (remove #{{:rule r, :complex c, :codomain cod}}
                                                   lm-vals))))
                            % cod-mates))
        ;; set l(a, x) = empty set
        (update-in [:lift-map]
                   #(reduce (fn remove-ax-from-lm [lm [a x]]
                              (update-in lm [a] dissoc x))
                            % mss))
        (update-in [:lift-map]
                   #(reduce (fn [lm a] (dissoc lm a)) % ras)))))

(defn- get-embeddings [mixture a-ids & rules]
  (for [r rules
        cr (-> r :lhs meta :complexes)
        :let [cr-expr (lang/subexpr (:lhs r) cr)
              cms (set (map #(lang/complex mixture (find mixture %)) a-ids))
              cm-exprs (map #(lang/subexpr mixture %) cms)]
        cm-expr cm-exprs
        :let [dom2cod (map (fn [[[[id1 _] s1] [[id2 _] s2]]]
                             [[id1 s1] [id2 s2]])
                           (lang/complex-dom2cod mixture [cr-expr] [cm-expr]))]
        :when (not (empty? dom2cod))]
    [r cr (into {} dom2cod)]))

(defn- positive-update [chamber]
  (let [ram (:ram chamber)
        mixture (:mixture chamber)
        m (meta chamber)
        activated-rules ((:ram chamber) (:executed-rule m))
        aas (:added-agents m)
        mss (:modified-sites m)
        ;; for every c in C(r') try to find a unique embedding
        ;; extension phi_c in [c S'] of the injection c -> {a}
        embs (apply get-embeddings mixture (concat aas (map first mss)) activated-rules)
        emb-agent-ids (for [[r cr cr2cm] embs]
                        [r cr (into {} (for [[[id1 _] [id2 _]] cr2cm]
                                         [id1 id2]))])
        cod-sites (for [[r cr cr2cm] embs
                        :let [cod (vals cr2cm)]
                        [a-id s] cod]
                    [a-id s r cr cod])]
    (-> chamber
        ;; for all obtained phi_cs add phi_c to Phi(r', c)
        (update-in [:matching-map]
                   #(reduce (fn update-mm [mm [r c emb]]
                              (update-in mm [r c] conj emb))
                            % emb-agent-ids))
        ;; and add <r', c, phi_c> to l(b, y) for all pairs (b, y) in cod(phi_c)
        (update-in [:lift-map]
                   #(reduce (fn update-lm [lm [a s r c cod]]
                              (update-in lm [a s] conj {:rule r, :complex c, :codomain cod}))
                            % cod-sites)))))

;; TODO obs-update can be optimized if the activation and inhibition maps are
;;      extended so rules can map to obs-exprs as well.
(defn- obs-update [chamber]
  (let [;;; obs-rules
        executed-rule (:executed-rule (meta chamber))
        obs-rules-updated (if (nil? ((:obs-rules chamber) executed-rule))
                            chamber
                            (update-in chamber [:obs-rules executed-rule] conj (:time chamber)))
        ;;; obs-exprs
        mixture (:mixture chamber)
        ;; positive update
        aas (:added-agents (meta chamber))
        mss (:modified-sites (meta chamber))
        mcs (set (for [a-id (concat aas (map first mss))] ; modified complexes
                   (lang/complex mixture (find mixture a-id))))
        embs (for [obs (keys (:obs-exprs chamber))
                   emb (filter (comp (partial lang/match obs)
                                     (partial lang/subexpr mixture)) mcs)]
               [obs emb])
        ;; negative update
        not-embs (for [obs (keys (:obs-exprs chamber))
                       not-emb (remove (comp (partial lang/match obs)
                                             (partial lang/subexpr mixture)) mcs)]
                   [obs not-emb])
        rcs (for [obs (keys (:obs-exprs chamber))
                  ra (:removed-agents (meta chamber))]
              [obs #{ra}])]
    (-> obs-rules-updated
        ;; positive update
        (update-in [:obs-exprs]
                   #(reduce (fn positive-update [om [obs emb]]
                              (-> (update-in om [obs] conj emb)
                                  (vary-meta into (for [a emb] [a [obs emb]]))))
                            % embs))
        ;; negative update
        (update-in [:obs-exprs]
                   #(reduce (fn negative-update [om [obs rc]]
                              (let [agent-complexes (meta om)]
                                (reduce (fn [om ra]
                                          (let [[obs-in-meta not-emb] (agent-complexes ra)]
                                            (if (and (not (nil? not-emb))
                                                     (= obs obs-in-meta))
                                              (-> (update-in om [obs] disj not-emb)
                                                  (vary-meta dissoc ra))
                                              om)))
                                        om rc)))
                            % (concat rcs not-embs))))))

(def *max-clashes* 8)

(defn gen-event
  "Generates a new event from chamber. You can additionally provide any
  function that takes two args, the chamber and the executed rule, and returns a
  new chamber. These callback functions are called at the end of the iteration and
  can update chamber's properties."
  [chamber & callbacks]
  (when (or (> (:clash-cnt chamber) *max-clashes*)
            (every? zero? (vals (:activity-map chamber))))
    (throw (Exception. "Deadlock found!")))
  (let [r (select-rule (:activity-map chamber))
        m (select-matching-per-complex ((:matching-map chamber) r))
        dt (time-advance (:activity-map chamber))]
    (if (clash? m)
      (-> chamber
        (update-in [:clash-cnt] inc) ; increment clash-cnt
        (update-in [:time] + dt) ; and time
        (update-in [:event-cnt] inc))
      (-> chamber
        ;; meta cleanup first... so after the event the user can review the meta info
        (vary-meta merge {:added-agents [], :removed-agents [], :modified-sites []})
        (vary-meta merge {:executed-rule r})
        ((:action r) m) negative-update positive-update obs-update
        (assoc :clash-cnt 0)
        (update-in [:time] + dt)
        (update-in [:event-cnt] inc)
        (update-activities (concat ((:ram chamber) r)
                                   ((:rim chamber) r) [r]))
        ((apply comp identity callbacks))))))

;; Talvez sea mejor realizar el positive y negative update en cada accion.

;; Hay que tener en mente que los compartimientos deben correr en paralelo y
;; debe existir una manera de pasar moleculas de un lado a otro y regresar en
;; el tiempo de ser necesario.

;; Deberia haber un agente que controle todos los compartimientos y conozca la
;; topologia del sistema. Es a este agente al cual se le envian las senales de
;; que una cierta molecula deberia aparecer en un compartimiento vecino.
;; Cada compartimiento deberia estar etiquetado y esta etiqueta ser el estado
;; interno del sitio 'loc' en los agentes que necesiten especificar su ubicacion.
;; Es el agente maestro realmente necesario?

;; La topologia del sistema queda definida implicitamente por las reglas.
;; Aquellos loc que aparecen en una misma regla estan conectados...
;; De hecho ni siquiera es necesario conocer la topologia para ejecutar el modelo

;; Debo crear una estructura que guarde el historial de cambios de un chamber
;; y que sea capaz de olvidarse de un cierto tiempo (que le llegara a traves
;; de un mensaje) hacia atras.

(defn get-obs-expr-counts
  "Get a map from observables (which are expressions) to the counts they have at
  each simulation step."
  [sim]
  (let [obs-exprs (set (mapcat (comp keys :obs-exprs) sim))]
    (into {} (for [obs obs-exprs]
               [obs (map (comp count #(% obs) :obs-exprs) sim)]))))


;;; Using Clojure futures to perform several simulations simultaneously

(defn psimulate [chamber num-steps num-simulations & callbacks]
  (for [sim (map deref
                 (doall (repeatedly num-simulations
                                    #(future (doall (take num-steps
                                                          (iterate gen-event chamber)))))))]
    {:time (map :time sim) :obs-expr-counts (get-obs-expr-counts sim)}))


;; TODO reachable-complexes and reachable-reactions
(defn reachable-complexes
  "Returns a lazy seq of all the complexes reachable by the system."
  [rule-set initial-state]
  nil)

(defn reachable-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system."
  [rule-set initial-state]
  nil)

