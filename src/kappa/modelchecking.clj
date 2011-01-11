(ns kappa.modelchecking
  {:doc "Functions to convert Kappa models into PRISM and NuSMV models."
   :author "Ricardo Honorato-Zimmer"}
  (:require [kappa.language :as lang]
            [kappa.reachability :as r]
            [kappa.parser :as p]
            [clojure.set :as set]
            [clojure.string :as s]
            [clojure.contrib.duck-streams :as ds]
            [clojure.contrib.shell-out :as sh-out]))

;;;; PRISM

(defn prism-model
  "Convert a kappa model into a PRISM model (for model-checking)"
  [rule-set initial-state & options]
  (let [complexes (r/reachable-complexes rule-set initial-state)
        reactions (r/get-reactions rule-set initial-state)
        module-names (for [complex complexes]
                       (apply str (interpose "-" (map :name complex))))]
    (doseq [m module-names]
      (println m))))

;;;; NuSMV

(defn- update-names [rules]
  (let [n2r (apply merge-with conj
                   (zipmap (set (map :name rules)) (repeat []))
                   (for [r rules]
                     {(:name r) r}))]
    (mapcat (fn [[name rs]]
              (for [[i r] (map vector (iterate inc 1) rs)]
                (assoc r :name (str name "-" i))))
            n2r)))

(defn- nusmv-expr-str [e]
  (str "\"" (lang/expr-str e) "\""))

(defn- assign [c-str reactants-cond reactant-or-product]
  [(str "    next(" c-str ") := case")
   (str "      " reactants-cond " : " (case reactant-or-product
                                        :reactant "1"
                                        :product "{1, 0}") ";")
   (str "      1 : " c-str ";")
   (str "    esac;")])

(defn- module [rxn arglist rhs-c-names lhs-c-names]
  (let [reactants-cond (apply str (interpose " & " lhs-c-names))]
    (concat [(str "MODULE " (:name rxn) "(" arglist ")")
             "  ASSIGN"]
            (mapcat #(assign % reactants-cond :product) rhs-c-names)
            (mapcat #(assign % reactants-cond :reactant) lhs-c-names)
            ["  FAIRNESS"
             "    running"])))

(defn- main-module [c-names initial-c-names reactions arglist]
  (concat ["MODULE main"
           "  VAR"]
          (for [c-str c-names]
            (str "    " c-str " : boolean;"))
          (for [[i rxn] (map vector (iterate inc 1) reactions)]
            (str "    r" i " : process " (:name rxn) "(" (arglist rxn) ");"))
          ["  ASSIGN"]
          (for [c-str initial-c-names]
            (str "    init(" c-str ") := 1;"))
          (for [c-str (set/difference (set c-names) (set initial-c-names))]
            (str "    init(" c-str ") := 0;"))))

(defn nusmv-model
  "Convert a kappa model into a boolean NuSMV model (for model-checking)"
  [rules initial-state]
  (let [complexes (r/reachable-complexes rules initial-state)
        c-names (map nusmv-expr-str complexes)
        initial-c-names (map nusmv-expr-str (lang/complexes initial-state))
        reactions (r/get-reactions (update-names rules) initial-state)
        lhs-c-names (into {} (for [rxn reactions]
                               [rxn (map nusmv-expr-str (lang/complexes (:lhs rxn)))]))
        rhs-c-names (into {} (for [rxn reactions]
                               [rxn (map nusmv-expr-str (lang/complexes (:rhs rxn)))]))
        arglist (into {} (for [rxn reactions]
                           [rxn (apply str (interpose ", "
                                                      (concat (lhs-c-names rxn)
                                                              (rhs-c-names rxn))))]))]
    {:nusmv (concat
             (mapcat #(module % (arglist %) (rhs-c-names %) (lhs-c-names %)) reactions)
             (main-module c-names initial-c-names reactions arglist))
     :c-names (zipmap complexes c-names)}))

(defn- interleave-all [s1 s2]
  (cond
    (= (count s1) (inc (count s2))) (concat (interleave s1 s2) [(last s1)])
    :else (interleave s1 s2)))

(defn- replace-complexes [spec sep c-names]
  (let [splitted-spec (s/split spec #"'")
        ss (take-nth 2 splitted-spec)
        c-strs (take-nth 2 (rest splitted-spec))
        cs (map p/parse-expr c-strs)
        cs2names (into {} (for [c cs]
                            [c (filter (partial lang/match-expr c) (keys c-names))]))]
    (if (some next (vals cs2names))
      (throw (Exception. "more than one match for spec complex"))
      (apply str (interleave-all ss (map (comp #(if (next %)
                                                  (str "(" (apply str (interpose " & " %)) ")")
                                                  (first %))
                                               (partial map c-names) cs2names) cs))))))

(defn- add-nusmv-specs [model specs type]
  (->> (concat (:nusmv model)
               (for [spec specs]
                 (str (case type
                        :LTL "  LTLSPEC "
                        :CTL "  SPEC ")
                      (apply str (replace-complexes spec "'" (:c-names model))) ";")))
       (assoc model :nusmv)))

(defn add-LTL-specs [model & LTL-specs]
  (add-nusmv-specs model LTL-specs :LTL))

(defn add-CTL-specs [model & CTL-specs]
  (add-nusmv-specs model CTL-specs :CTL))

(defn nusmv-check [model]
  (let [f (java.io.File/createTempFile "nusmv-model-" ".smv")]
    (ds/write-lines f (:nusmv model))
    (-> (sh-out/sh "nusmv" (.getPath f))
        (s/split-lines))))

(defn print-lines [ss]
  (doseq [s ss]
    (println s)))

