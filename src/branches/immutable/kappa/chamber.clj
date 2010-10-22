(ns kappa.chamber
  (:use kappa.maps
        [kappa.misc :only (choice)]
        [clojure.contrib.math :only (expt)])
  (:require [clojure.contrib.generic.math-functions :as math]))

;;; Chambers
;; me gustaria que al terminar cada paso de simulacion se pudiese llamar una funcion
;; callback sobre los cambios y/o el estado nuevo de la simulacion
;; esta funcion se le podria pasar a la funcion que haga la iteracion
;; esta funcion podria servir para actualizar el volumen u otras partes del Chamber
(defrecord Chamber [rules mixture
                    time event-cnt clash-cnt
                    volume stochastic-kcs activities ;; FIXME activities should be activity-map
                    matching-map lift-map ram rim])

(defn update-volume [chamber new-volume]
  (assoc chamber :volume (with-meta new-volume
                           {:changed? true})))

(defn determ2stoch [volume rule]
  (let [Avogadro 6.022e23
        num-complexes (count (:lhs rule))]
    (/ (* (expt volume (if (= num-complexes 0) 0
                           (- num-complexes 1)))
          (:rate rule)
          Avogadro)
       (:automorphisms (meta (:lhs rule))))))

(defn update-stochastic-kcs [chamber]
  (if (:changed? (meta (:volume chamber)))
    (-> chamber
        (assoc :volume (with-meta (:volume chamber) {:changed? false}))
        (assoc :stochastic-kcs (with-meta (map (partial determ2stoch (:volume chamber))
                                               (:rules chamber))
                                 {:changed? true})))
    chamber))

(defn activity [ms k aut] ; matchings and stochastic kinetic constants
  (/ (apply * k (map #(count (val %)) ms)) aut))

(defn update-activities [chamber]
  (if (:changed? (meta (:stochastic-kcs chamber)))
    (let [mm (:matching-map chamber), kcs (:stochastic-kcs chamber)]
      (-> chamber
          (assoc :stochastic-kcs (with-meta kcs {:changed? false}))
          (assoc :activities (map #(activity (mm %) (kcs %) (-> % :lhs meta :automorphisms))
                                  (:rules chamber)))))
    chamber))

(defn create-chamber [rule-set mixture volume time]
  (let [[mm lf] (matching-and-lift-map rule-set mixture)]
    (-> (Chamber. rule-set mixture time 0 0 ; rules, mixture and counters
                  (with-meta volume {:changed? true}) [] [] ; kcs and activities are computed later
                  mm lf (activation-map rule-set) (inhibition-map rule-set))
        update-stochastic-kcs
        update-activities)))

(defn time-advance [activities]
  (/ (math/log (/ 1 (rand)))
     (apply + (vals activities))))

(defn select-rule [activities]
  (choice activities))

(defn select-matching [matchings]
  (apply merge (for [[c m] matchings]
                 {c (choice m)})))

(defn clash? [m]
  (let [agents (for [c m, a c] a)]
    (every? #(= (count %) 1)
            (map #(filter #{%} agents) agents))))

(defn negative-update [chamber] nil)
(defn positive-update [chamber] nil)

(defn gen-event
  "Generates a new event from chamber. You can additionally provide any
  function that takes two args, the chamber and the executed rule, and returns
  a new chamber. These callback functions are called at the end of the iteration
  and can communicate with the organizer."
  [chamber & callbacks]
  (let [updated-chamber (update-activities chamber)
        r (select-rule (:activities updated-chamber))
        m (select-matching ((:matching-map updated-chamber) r))]
    (if (clash? m)
      (update-in updated-chamber [:clash-cnt] inc) ;; increment clash-cnt and exit
      (let [dt (time-advance (:activities updated-chamber))
            action (:action r)]
        (-> updated-chamber
            ;; meta cleanup first... so after the event the user can review the meta info
            (vary-meta merge {:added-agents [], :removed-agents [], :modified-agents []})
            (partial action m) negative-update positive-update
            (update-in [:time] #(+ % dt))
            (vary-meta merge {:executed-rule r})
            ((apply comp callbacks)))))))

;; How can I have multiple mutually-referencing objects in one atom?

;; Tengo que encontrar una manera de guardar los cambios que realiza action
;; sobre la solucion, para asi despues poder pasarselo a negative-update,
;; positive-update y los callbacks.
;; Creo que la mejor manera es anhadiendo metadata al chamber cuando ejecuto
;; la accion de la regla.

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

