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

;;; Constructor
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
  (let [fake-rule (lang/make-rule "obs-exprs" (apply lang/mix-exprs obs-exprs) {} 1)
        rules+obs-exprs (conj rules fake-rule)
        [mm lf] (maps/matching-and-lift-map rules+obs-exprs mixture)]
    (case rate-type
      :deterministic
      (-> (DeterministicRateChamber. rules mixture 0 0 0 volume
                                     [] [] ; stochastic cs and activities are computed later
                                     mm lf (maps/activation-map rules+obs-exprs)
                                     (maps/inhibition-map rules+obs-exprs)
                                     fake-rule (zipmap obs-rules (repeat []))) ; observables
        (update-stochastic-cs)
        (update-activities))

      :stochastic
      (-> (StochasticRateChamber. rules mixture 0 0 0 [] ; activities are computed later
                                  mm lf (maps/activation-map rules+obs-exprs)
                                  (maps/inhibition-map rules+obs-exprs)
                                  fake-rule (zipmap obs-rules (repeat []))) ; observables
        (update-activities)))))

;;; Events
(defn- time-advance [activities]
  (/ (math/log (/ 1 (rand)))
     (apply + (vals activities))))

(defn- select-rule [activities]
  (misc/choice activities))

(defn- select-matching-per-complex [mm]
  (apply merge (for [[c m] mm]
                 {c (rand-nth (seq m))})))

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
        r-c-embs (apply concat (concat
                                (for [[ma ms] mss]
                                  ((lift-map ma) ms))
                                (for [ra ras]
                                  (apply concat (vals (lift-map ra))))))
        cod-mates (for [{r :rule, c :complex, emb :emb} r-c-embs
                        [_ [cod-id cod-site]] emb]
                    [cod-id cod-site r c emb])]
    (-> chamber
      ;; remove phi_c from Phi(r', c)
      (update-in [:matching-map]
                 #(reduce (fn remove-phi [mm {r :rule, c :complex, emb :emb}]
                            (let [matching (into {} (for [[[dom-id _] [cod-id _]] emb]
                                                      [dom-id cod-id]))]
                              (update-in mm [r c] disj matching)))
                          % r-c-embs))
      ;; for all pairs (b, y) in cod(phi_c) remove <r', c, phi_c> from l(b, y)
      (update-in [:lift-map]
                 #(reduce (fn lm-update [lm [a-id s r c emb]]
                            (update-in lm [a-id s]
                                       (fn [lm-vals]
                                         (remove #{{:rule r, :complex c, :emb emb}} lm-vals))))
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
        :let [embs (lang/domain2codomain mixture [cr-expr] [cm-expr])]
        :when (not (empty? embs))]
    [r cr (into {} embs)]))

(defn- positive-update [chamber]
  (let [ram (:ram chamber)
        mixture (:mixture chamber)
        m (meta chamber)
        activated-rules ((:ram chamber) (:executed-rule m))
        aas (:added-agents m)
        mss (:modified-sites m)
        ;; for every c in C(r') try to find a unique embedding
        ;; extension phi_c in [c S'] of the injection c -> {a}
        r-c-embs (apply get-embeddings mixture (concat aas (map first mss)) activated-rules)
        emb-agent-ids (for [[r cr emb] r-c-embs]
                        [r cr (into {} (for [[[id1 _] [id2 _]] emb]
                                         [id1 id2]))])
        cod-sites (for [[r cr emb] r-c-embs
                        [cod-id cod-site] (vals emb)]
                    [cod-id cod-site r cr emb])]
    (-> chamber
      ;; for all obtained phi_cs add phi_c to Phi(r', c)
      (update-in [:matching-map]
                 #(reduce (fn update-mm [mm [r c emb]]
                            (update-in mm [r c] conj emb))
                          % emb-agent-ids))
      ;; and add <r', c, phi_c> to l(b, y) for all pairs (b, y) in cod(phi_c)
      (update-in [:lift-map]
                 #(reduce (fn update-lm [lm [a s r c emb]]
                            (update-in lm [a s] conj {:rule r, :complex c, :emb emb}))
                          % cod-sites)))))

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
  (let [r (select-rule (select-keys (:activity-map chamber) (:rules chamber))) ;; we don't want to select the fake rule
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
        ((:action r) m) negative-update positive-update
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
  [chamber]
  (let [obs-exprs-rule (:obs-exprs chamber)
        obs-exprs (:lhs obs-exprs-rule)]
    (into {} (for [obs (-> obs-exprs meta :complexes)]
               [(lang/subexpr obs-exprs obs)
                (count (((:matching-map chamber) obs-exprs-rule) obs))]))))

(defn get-sim-counts
  [sim]
  (let [counts (map get-obs-expr-counts sim)
        obs-exprs (set (mapcat keys counts))]
    (into {} (for [obs obs-exprs]
               [obs (map #(% obs) counts)]))))

(defrecord SimulationResult [time obs-expr-counts])

(defn iter
  "Returns the infinite sequence of consecutive states of a stochastic simulation.
  The initial conditions for the simulation are given in chamber.
  Each state (i.e., each element of the sequence) is a record with the fields time
  and obs-expr-counts. The former contains the point in time in which the state is
  generated and the latter contains the number of times each observed expression
  is found in that state."
  [chamber & callbacks]
  (cons (SimulationResult. (:time chamber)
                           (get-obs-expr-counts chamber))
        (lazy-seq
          (apply iter (apply gen-event chamber callbacks) callbacks))))

(defn simulate
  "Returns a record with the fields time and obs-expr-counts. But, in contrast with
  kappa.chamber/iter, these fields contain the information of the whole simulation,
  not each time step. This means the time field contains the sequence of all time
  points at which events ocurred during the simulation. Likewise, the obs-expr-counts
  field contains a map from observed expressions to the sequence of their counts."
  [chamber num-steps & callbacks]
  (let [result (take num-steps (apply iter chamber callbacks))
        obs-exprs (keys (:obs-expr-counts (first result)))]
    (SimulationResult. (map :time result)
                       (into {} (for [obs obs-exprs]
                                  [obs (map (comp #(% obs) :obs-expr-counts) result)])))))


;;; Using Clojure futures to perform several simulations simultaneously

(defn psimulate
  "Repeateadly call the simulate function in different threads."
  [chamber num-steps num-simulations & callbacks]
  (map deref
       (doall (repeatedly num-simulations
                          #(future (doall (apply simulate num-steps chamber callbacks)))))))


;; TODO reachable-complexes and reachable-reactions
(defn reachable-complexes
  "Returns a lazy seq of all the complexes reachable by the system."
  [rule-set initial-state]
  nil)

(defn reachable-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system."
  [rule-set initial-state]
  nil)

