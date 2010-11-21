(ns ^{:doc "Chambers are the places where reactions occur."
      :author "Ricardo Honorato-Zimmer"}
  kappa.chamber
  (:require [kappa.maps :as maps]
            [kappa.misc :as misc]
            [clojure.set :as set]
            [kappa.language :as lang]
            [clojure.contrib.math :as m]
            [clojure.contrib.generic.math-functions :as math]))

;;; Chambers
(defrecord Chamber [rules mixture time event-cnt clash-cnt
                    volume stochastic-cs activities
                    matching-map lift-map ram rim obs-exprs obs-rules])

(defn- update-volume [chamber new-volume]
  (-> chamber
      (assoc :volume new-volume)
      (vary-meta assoc :volume-changed? true)))

(defn- determ2stoch [volume rule]
  (let [n-avogadro 6.022e23
        num-complexes (-> rule :lhs meta :complexes count) ;; reaction's order
        rate (:rate rule)
        aut (-> rule :lhs meta :automorphisms)]
    (cond
      (zero? num-complexes) (/ (* rate volume) n-avogadro)
      (= num-complexes 1) rate
      (> num-complexes 1) (/ (* rate aut (m/expt n-avogadro (dec num-complexes)))
                             (m/expt volume (dec num-complexes))))))

(defn- update-stochastic-cs [chamber]
  (if (:volume-changed? (meta chamber))
    (-> chamber
        (vary-meta assoc :volume-changed? false)
        (assoc :stochastic-cs (zipmap (:rules chamber)
                                      (map (partial determ2stoch (:volume chamber))
                                           (:rules chamber))))
        (vary-meta assoc :kcs-changed? true))
    chamber))

(defn- activity [ms k aut] ; matchings (ms) and stochastic kinetic constant (k)
  (/ (apply * k (map (comp count val) ms)) aut))

(defn- update-activities
  ([chamber]
     (let [updated-chamber (update-stochastic-cs chamber)]
       (if (:kcs-changed? (meta chamber))
         (let [mm (:matching-map chamber)
               kcs (:stochastic-cs chamber)
               rules (:rules chamber)]
           (-> chamber
               (vary-meta assoc :kcs-changed? false)
               (assoc :activities
                 (into {} (map (fn [r]
                                 [r (activity (mm r) (kcs r) (-> r :lhs meta :automorphisms))])
                               rules)))))
         chamber)))
  ([chamber rules]
     (let [mm (:matching-map chamber)
           cs (:stochastic-cs chamber)]
       (update-in chamber [:activities]
                  #(reduce (fn [am r]
                             (assoc am r (activity (mm r) (cs r)
                                                   (-> r :lhs meta :automorphisms))))
                           % rules)))))

(defn make-chamber
  ([rules mixture volume obs-exprs]
     (make-chamber rules mixture volume 0 obs-exprs []))
  ([rules mixture volume obs-exprs obs-rules]
     (make-chamber rules mixture volume 0 obs-exprs obs-rules))
  ([rules mixture volume time obs-exprs obs-rules]
     (let [[mm lf] (maps/matching-and-lift-map rules mixture)]
       (-> (Chamber. rules mixture time 0 0 0 [] [] ; kcs and activities are computed later
                     mm lf (maps/activation-map rules) (maps/inhibition-map rules) ; maps
                     (maps/obs-map obs-exprs mixture) (zipmap obs-rules (repeat {}))) ; observables
           (update-volume volume)
           (update-stochastic-cs)
           (update-activities)))))

(defn- time-advance [activities]
  (/ (math/log (/ 1 (rand)))
     (apply + (vals activities))))

(defn- select-rule [activities]
  (misc/choice activities))

(defn select-matching-per-complex [mm]
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

(defn- positive-update [chamber]
  (let [ram (:ram chamber)
        mixture (:mixture chamber)
        m (meta chamber)
        activated-rules ((:ram chamber) (:executed-rule m))
        aas (:added-agents m)
        mss (:modified-sites m)
        cm-exprs (set (for [a-id (concat aas (map first mss))
                            :let [cm (lang/complex mixture [a-id (mixture a-id)])]]
                        (with-meta (lang/subexpr mixture cm)
                          {:complexes [cm]})))
        ;; for every c in C(r') try to find a unique embedding
        ;; extension phi_c in [c, S'] of the injection c -> {a}
        embs (remove (comp empty? misc/third)
                     (for [r activated-rules
                           cr (-> r :lhs meta :complexes)
                           :let [cr-expr (with-meta (lang/subexpr (:lhs r) cr)
                                           {:complexes [cr]})]
                           cm-expr cm-exprs]
                       [r cr (into {} (map (fn [[[[id1 a1] s1] [[id2 a2] s2]]]
                                             [[id1 s1] [id2 s2]])
                                           (maps/codomain cr-expr cm-expr)))]))
        cods (map (fn [[r cr cr->cm]]
                    [r cr (vals cr->cm)]) embs)
        emb-agent-ids (map (fn [[r cr cr->cm]]
                             [r cr (into {} (map (fn [[[id1 s1] [id2 s2]]]
                                                   [id1 id2]) cr->cm))]) embs)
        cod-mates (for [[r cr cod] cods
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
                            % cod-mates)))))

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
                   (lang/complex mixture [a-id (mixture a-id)])))
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
                   #(reduce (fn [om [obs emb]]
                              (-> (update-in om [obs] conj emb)
                                  (vary-meta into (for [a emb] [a [obs emb]]))))
                            % embs))
        ;; negative update
        (update-in [:obs-exprs]
                   #(reduce (fn [om [obs rc]]
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
  can update chamber's properties (e.g. volume) or communicate with the organizer."
  [chamber & callbacks]
  (when (or (> (:clash-cnt chamber) *max-clashes*)
            (every? zero? (vals (:activities chamber))))
    (throw (Exception. "Deadlock found!")))
  (let [r (select-rule (:activities chamber))
        m (select-matching-per-complex ((:matching-map chamber) r))
        dt (time-advance (:activities chamber))]
    (if (clash? m)
      (-> chamber
          (update-in [:clash-cnt] inc) ; increment clash-cnt
          (update-in [:time] + dt)) ; and time
      (-> chamber
          ;; meta cleanup first... so after the event the user can review the meta info
          (vary-meta merge {:added-agents [], :removed-agents [], :modified-sites []})
          (vary-meta merge {:executed-rule r})
          ((:action r) m) negative-update positive-update obs-update
          (assoc :clash-cnt 0)
          (update-in [:time] + dt)
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
  (apply merge-with concat
         (for [step sim
               [obs m] (:obs-exprs step)]
           {obs [(count m)]})))


;; TODO reachable-complexes and reachable-reactions
(defn reachable-complexes
  "Returns a lazy seq of all the complexes reachable by the system."
  [rule-set initial-state]
  nil)

(defn reachable-reactions
  "Returns a lazy seq of all the rule instances (i.e., reactions) reachable by the system."
  [rule-set initial-state]
  nil)

