(ns kappa
  (:use kappa.language kappa.interp kappa.chamber
        [kappa.maps :only (activation-map inhibition-map matching-and-lift-map)]
        :reload-all)
  (:import (kappa.language Agent Rule)))

