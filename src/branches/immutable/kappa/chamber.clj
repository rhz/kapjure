(ns ^{:doc ""
      :author "Ricardo Honorato-Zimmer"}
  kappa.chamber
  (:require [kappa.maps :as maps]
            [kappa.misc :as misc]
            [clojure.contrib.math :as m]
            [clojure.contrib.generic.math-functions :as math]))

;;; Chambers
(defrecord Chamber [rules mixture
                    time event-cnt clash-cnt
                    volume stochastic-kcs activities ;; FIXME activities should be activity-map
                    matching-map lift-map ram rim observables])

(defn- update-volume [chamber new-volume]
  (-> chamber
      (assoc :volume new-volume)
      (vary-meta assoc-in [:volume-changed?] true)))

(defn- determ2stoch [volume rule]
  (let [n-avogadro 6.022e23
        num-complexes (count (:lhs rule)) ; num-complexes = reaction's order
        rate (:rate rule)
        aut (-> rule :lhs meta :automorphisms)]
    (cond
      (zero? num-complexes) (/ (* rate volume) n-avogadro)
      (= num-complexes 1) rate
      (> num-complexes 1) (/ (* rate aut (m/expt n-avogadro (dec num-complexes)))
                             (m/expt volume (dec num-complexes))))))

(defn- update-stochastic-kcs [chamber]
  (if (:volume-changed? (meta chamber))
    (-> chamber
        (vary-meta assoc-in [:volume-changed?] false)
        (assoc :stochastic-kcs (zipmap (:rules chamber)
                                       (map (partial determ2stoch (:volume chamber))
                                            (:rules chamber))))
        (vary-meta assoc-in [:kcs-changed?] true))
    chamber))

(defn- activity [ms k aut] ; matchings and stochastic kinetic constants
  (/ (apply * k (map #(count (val %)) ms)) aut))

(defn- update-activities [chamber]
  (if (:kcs-changed? (meta chamber))
    (let [mm (:matching-map chamber), kcs (:stochastic-kcs chamber)]
      (-> chamber
          (vary-meta assoc-in [:kcs-changed?] false)
          (assoc :activities (map #(activity (mm %) (kcs %) (-> % :lhs meta :automorphisms))
                                  (:rules chamber)))))
    chamber))

(defn make-chamber [rule-set mixture volume time observables]
  (let [[mm lf] (maps/matching-and-lift-map rule-set mixture)]
    (-> (Chamber. rule-set mixture time 0 0 volume [] [] ; kcs and activities are computed later
                  mm lf (maps/activation-map rule-set) (maps/inhibition-map rule-set) observables)
        (with-meta {:volume-changed? true})
        update-stochastic-kcs
        update-activities)))

(defn- time-advance [activities]
  (/ (math/log (/ 1 (rand)))
     (apply + (vals activities))))

(defn- select-rule [activities]
  (misc/choice activities))

(defn select-matching [matchings]
  (apply merge (for [[c m] matchings]
                 {c (misc/choice m)})))

(defn- clash? [m]
  (let [agents (for [c m, a c] a)]
    (every? #(= (count %) 1)
            (map #(filter #{%} agents) agents))))

(defn- negative-update [chamber]
  chamber)

(defn- positive-update [chamber]
  chamber)

(defn gen-event
  "Generates a new event from chamber. You can additionally provide any
  function that takes two args, the chamber and the executed rule, and returns a
  new chamber. These callback functions are called at the end of the iteration and
  can update chamber's properties (e.g. volume) or communicate with the organizer."
  [chamber & callbacks]
  (let [updated-chamber (update-activities chamber)
        r (select-rule (:activities updated-chamber))
        m (select-matching ((:matching-map updated-chamber) r))]
    (if (clash? m)
      (update-in updated-chamber [:clash-cnt] inc) ; increment clash-cnt and exit
      (let [dt (time-advance (:activities updated-chamber))
            action (:action r)]
        (-> updated-chamber
            ;; meta cleanup first... so after the event the user can review the meta info
            (vary-meta merge {:added-agents [], :removed-agents [], :modified-agents []})
            (action m) negative-update positive-update
            (update-in [:time] #(+ % dt))
            (vary-meta merge {:executed-rule r})
            ((apply comp callbacks)))))))

;; Tengo que encontrar una manera de guardar los cambios que realiza action
;; sobre la solucion, para asi despues poder pasarselo a negative-update,
;; positive-update y los callbacks.
;; Creo que la mejor manera es anhadiendo metadata al chamber cuando ejecuto
;; la accion de la regla.
;; Talvez lo mejor sea realizar el positive y negative update en cada accion.

;; Hay que tener en mente que los compartimientos deben correr en paralelo y
;; debe existir una manera de pasar moleculas de un lado a otro y regresar en
;; el tiempo de ser necesario.

;; Deberia haber un agente que controle todos los compartimientos y conozca la
;; topologia del sistema. Es a este agente al cual se le envian las senales de
;; que una cierta molecula deberia aparecer en un compartimiento vecino.
;; Cada compartimiento deberia estar etiquetado y esta etiqueta ser el estado
;; interno del sitio 'loc' en los agentes que necesiten especificar su ubicacion.

;; La topologia del sistema queda definida implicitamente por las reglas.
;; Aquellos loc que aparecen en una misma regla estan conectados...
;; De hecho ni siquiera es necesario conocer la topologia para ejecutar el modelo

;; Existe alguna manera de guardar el historial de cambios de un agente/ref/etc?
;; Chamber esta pensado para ser contenido dentro de agentes.

