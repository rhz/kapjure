(ns kappa.simple-models
  (:require [kappa.language :as lang]
            [kappa.chamber :as c]
            [kappa.parser :as p]
            [kappa.graphics :as g]
            :reload)
  (:gen-class))


(defn lotka-volterra [num-steps num-simulations]
  (p/let-rules [replication "X(x), Y1(x) -> Y1(x), Y1(x), X(x) @ 1" ; X is food
                predation "Y1(x), Y2(x) -> Y2(x), Y2(x) @ 0.01" ; Y1 is the prey
                death "Y2(x) -> Z(x) @ 10"] ; and Y2 is the predator
    (p/let-exprs [initial-mixture "1000 * Y1(x), 1000 * Y2(x), 10 * X(x)"
                  Y1 "Y1(x)" Y2 "Y2(x)"]
      (let [chamber (c/make-stochastic-chamber [replication predation death] initial-mixture [Y1 Y2])
            results (c/psimulate chamber num-steps num-simulations)
            rpd (inc (quot num-steps 500))] ; to plot just 500 points... FIXME is this right?
        (g/get-avg-result results :rpd rpd)))))


(defn brusselator [num-steps num-simulations]
  (p/let-rules [r1 "X1(x) -> Y1(x), X1(x) @ 1000"
                r2 "X2(x), Y1(x) -> Y2(x), Z1(x), X2(x) @ 10"
                r3 "Y1(x), Y1(x), Y2(x) -> Y1(x), Y1(x), Y1(x) @ 0.00005"
                r4 "Y1(x) -> Z2(x) @ 5"]
    (p/let-exprs [initial-mixture "1000 * Y1(x), 2000 * Y2(x), 5 * X1(x), 5 * X2(x)"
                  Y1 "Y1(x)" Y2 "Y2(x)"]
      (let [chamber (c/make-stochastic-chamber [r1 r2 r3 r4] initial-mixture [Y1 Y2])
            results (c/psimulate chamber num-steps num-simulations)
            rpd (inc (quot num-steps 500))]
        (g/get-avg-result results :rpd rpd)))))


(defn oreganator [num-steps num-simulations]
  (p/let-rules [r1 "X1(x), Y2(x) -> Y1(x), X1(x) @ 0.2"
                r2 "Y1(x), Y2(x) -> Z1(x) @ 0.1"
                r3 "X2(x), Y1(x) -> Y1(x), Y1(x), Y3(x), X2(x) @ 10.4"
                r4 "Y1(x), Y1(x) -> Z2(x) @ 0.016"
                r5 "X3(x), Y3(x) -> Y2(x), X3(x) @ 2.6"]
    (p/let-exprs [initial-mixture "500 * Y1(x), 1000 * Y2(x), 2000 * Y3(x), 10 * X1(x), 10 * X2(x), 10 * X3(x)"
                  Y1 "Y1(x)" Y2 "Y2(x)" Y3 "Y3(x)"]
      (let [chamber (c/make-stochastic-chamber [r1 r2 r3 r4 r5] initial-mixture [Y1 Y2 Y3])
            results (c/psimulate chamber num-steps num-simulations)
            rpd (inc (quot num-steps 500))]
        (g/get-avg-result results :rpd rpd)))))


(defn futile-cycle [num-steps num-simulations]
  (p/let-rules [r1 "S(s~y), G(s~gdp) -> S(s~y!1), G(s~gdp!1) @ 6.3E-7"
                r2 "S(s~n), G(s~gtp) -> S(s~n!1), G(s~gtp!1) @ 6.3E-7"
                r3 "G(s~gdp!1), S(s~y!1) -> G(s~gtp!1), S(s~y!1) @ 0.4"
                r4 "G(s~gtp!1), S(s~n!1) -> G(s~gdp!1), S(s~n!1) @ 0.4"
                r5 "S(s!_) -> S(s) @ 0.23"]
    (p/let-exprs [initial-mixture "1000000 * G(s~gdp), 11000 * S(s~y), 89000 * S(s~n)"
                  Ggdp "G(s~gdp)" Ggtp "G(s~gtp)"]
      (let [chamber (c/make-stochastic-chamber [r1 r2 r3 r4 r5] initial-mixture [Ggdp Ggtp])
            results (c/psimulate chamber num-steps num-simulations)
            rpd (inc (quot num-steps 500))]
        (g/get-avg-result results :rpd rpd)))))


(defn isomerization [num-steps num-simulations]
  (p/let-rules [r1 "A(x) -> B(x) @ 1"]
    (p/let-exprs [initial-mixture "1000000 * A(x)"
                  A "A(x)" B "B(x)"]
      (let [chamber (c/make-stochastic-chamber [r1] initial-mixture [A B])
            results (c/psimulate chamber num-steps num-simulations)
            rpd (inc (quot num-steps 500))]
        (g/get-avg-result results :rpd rpd)))))


(defn -main [num-steps num-simulations]
  (read-line)
  (let [result (time (lotka-volterra (Integer/parseInt num-steps)
                                     (Integer/parseInt num-simulations)))]
    (shutdown-agents)
    (read-line)
    (time (g/view (g/plot-result result)))))

