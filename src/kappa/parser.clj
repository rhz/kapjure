(ns kappa.parser
  {:doc "Functions and macros to parse Kappa agents, expressions, rules and systems."
   :author "Ricardo Honorato-Zimmer"}
  (:require [edu.arizona.fnparse.hound :as h]
            [clojure.contrib [except :as except]]
            [kappa.language :as lang]
            [kappa.misc :as misc]
            [kappa.chamber :as chamber]
            [clojure.set :as set])
  (:use [clojure.template :only [do-template]]))

(defn str* [objects]
  (apply str objects))

;;; Whitespace
(do-template [rule-name label token]
  (h/defrule- rule-name
    (h/label label (h/lit token)))
  <space> "a space character" \space
  <tab> "a tab character" \tab
  <nl> "a newline character" \newline
  <return> "a return character" \return)

(h/defrule- <line-break> (h/rep (h/+ <nl> <return>)))

(h/defrule- <comment>
  "Consumes a comment."
  {:success "When there's a # followed by any string and an optional line break."
   :product "A namespace-qualified :comment keyword."}
  (h/label "a comment"
           (h/chook ::comment (h/cat (h/lit \#) (h/rep* (h/+ h/<ascii-alphanumeric> <space>))
                                     (h/opt <line-break>)))))

(h/defrule- <ws>
  "Consumes any amount of whitespace characters and returns a namespace-qualified :ws keyword."
  (h/chook ::ws (h/rep (h/+ <space> <tab> <line-break> <comment>))))

(h/defmaker- concat-nested-rules
  {:product "A string containing all the characters consumed by the given rules put in a `h/cat`."
   :success "When the concatenated rules succeed and they all return only seqs of chars (which can be nested)."}
  [& rules]
  {:pre #{(every? h/rule? rules)}}
  (h/hook (comp str* flatten) (apply h/cat rules)))

(h/defrule- <alphanumeric-string>
  (h/label "an alphanumeric string"
           (concat-nested-rules (h/rep h/<ascii-alphanumeric>))))

;;; Numbers
(h/defrule- <number-sign>
  (h/label "a number sign"
           (h/+ (h/lit \+) (h/lit \-))))

(h/defrule- <integer-part>
  (h/label "an integer part"
           (h/cat (h/opt <number-sign>) (h/rep h/<ascii-digit>))))
  
(h/defrule- <fractional-part>
  (h/label "a fractional part"
           (h/cat (h/lit \.) (h/rep* h/<ascii-digit>))))

(h/defrule- <exponential-part>
  (h/label "an exponential part"
           (h/cat (h/case-insensitive-lit \e) <integer-part>)))

(h/defrule- <integer-number>
  (h/hook read-string (concat-nested-rules <integer-part>)))

(h/defrule- <decimal-number>
  (h/hook read-string (concat-nested-rules <integer-part>
                                           (h/opt <fractional-part>)
                                           (h/opt <exponential-part>))))

(h/defrule- <ratio>
  (h/hook read-string (concat-nested-rules (h/lex <integer-part>) (h/lit \/) <integer-part>)))

(h/defrule- <number>
  (h/+ <ratio> <decimal-number>))

;;; Agent interface
(h/defrule- <internal-state>
  (h/prefix (h/lit \~) <alphanumeric-string>))

(h/defrule- <binding-state>
  (h/prefix (h/lit \!) (h/+ <integer-number>
                            (h/chook :unspecified (h/lit \?))
                            (h/chook :semi-link (h/lit \_)))))

(h/defrule- <site>
  (h/cat <alphanumeric-string> (h/opt <internal-state>) (h/opt <binding-state>)))

(h/defrule- <sep-and-ws>
  (h/cat (h/lit \,) (h/rep* <ws>)))

(defn- make-interface [sites]
  (let [[site-names states bindings] (apply map vector sites)
        states (map #(or % "") states)
        bindings (map #(or % :free) bindings)]
    [(zipmap site-names states) (zipmap site-names bindings)]))

(h/defrule- <interface>
  "Consumes the interface of a Kappa agent and returns a seq with
  the states and bindings map."
  (h/hook make-interface
          (h/circumfix (h/lit \() (h/separated-rep* <sep-and-ws> <site>) (h/lit \)))))

;;; Agent
(h/defrule- <agent>
  "Consumes a Kappa agent."
  (h/for [name (concat-nested-rules (h/cat h/<ascii-letter> (h/rep* h/<ascii-alphanumeric>)))
          [states bindings] <interface>]
    (lang/make-agent name states bindings)))

;;; Expression
(h/defmaker- circumfix-ws [rule]
  (h/circumfix (h/opt <ws>) rule (h/opt <ws>)))

(defn- replace-in-vals [m [& ks] smap]
  (update-in m ks #(zipmap (keys %) (replace smap (vals %)))))

(defn- get-neighbours [expr]
  (let [b2a (apply merge-with concat
                   (for [[id a] expr
                         [site-name binding] (:bindings a)]
                     (when (number? binding)
                       {binding [[id site-name]]})))]
    (if (every? #(= (count %) 2) (vals b2a))
      (apply merge-with into
             (for [[binding [[id1 sn1] [id2 sn2]]] b2a]
               {id1 {sn1 [id2 sn2]}, id2 {sn2 [id1 sn1]}}))
      (throw (Exception. (str "Could not parse expression: " expr))))))

(defn- if-intersects-unify [first-nbs]
  (loop [nbs (set first-nbs)]
    (let [second-nbs (apply set/union (map #(if (empty? (set/intersection %1 %2))
                                              #{%1 %2}
                                              #{(set/union %1 %2)}) nbs nbs))]
      (if (= nbs second-nbs)
        second-nbs
        (recur second-nbs)))))

(defn- get-complexes [expr nbs]
  (let [first-nbs (for [[id m] nbs]
                    (apply hash-set id (map first (vals m))))
        unbound-agents (set/difference (set (keys expr)) (set (keys nbs)))
        cs (if-intersects-unify first-nbs)]
    (concat (map hash-set unbound-agents) cs)))

(defn- replace-bindings [expr nbs]
  (reduce (fn [e [id1 m]]
            (reduce (fn [e [sn1 [id2 sn2]]]
                      (assoc-in e [id1 :bindings sn1] id2))
                    e m))
          expr nbs))

(defn- make-expr [agents-and-subexprs]
  (let [[agents subexprs] ((juxt filter remove) lang/agent? agents-and-subexprs)
        ;; expression with bond labels instead of neighbour id
        e1 (zipmap (repeatedly misc/counter) agents)
        nbs (get-neighbours e1)
        subexpr-complexes (map (comp :complexes meta) subexprs)]
    (with-meta (apply merge (replace-bindings e1 nbs) subexprs)
      {:complexes (apply concat (get-complexes e1 nbs) subexpr-complexes)})))

(defn- rep-expr [factor subexpr] ;; replicate expression
  (let [agents (vals subexpr), template-ids (keys subexpr),
        complexes (-> subexpr meta :complexes),
        new-ids (repeatedly (or factor 1) #(repeatedly (count agents) misc/counter)),
        smaps (map #(zipmap template-ids %) new-ids)] ;; substitution maps
    (vector
     (with-meta (apply merge
                       (map (fn [ids sm]
                              (zipmap ids (for [a agents]
                                            (replace-in-vals a [:bindings] sm))))
                            new-ids smaps))
       {:complexes (mapcat #(map (comp set (partial replace %)) complexes) smaps)}))))

(declare <expr>)
(h/defrule- <expr>
  "Consumes a Kappa expression."
  {:no-memoize? true}
  (h/hook (comp make-expr (partial apply concat))
          (h/separated-rep*
           <sep-and-ws>
           (h/for [factor (h/opt (h/suffix <integer-number> (circumfix-ws (h/lit \*))))
                   
                   result (h/+ (h/hook #(repeat (or factor 1) %) <agent>)
                               (h/hook #(rep-expr (or factor 1) %)
                                       (h/circumfix (h/lit \() (circumfix-ws <expr>) (h/lit \)))))]
             result))))

;;; Rule
(h/defrule- <arrow>
  (h/template-sum [label s product]
    (h/label label (h/chook product (h/phrase s)))
    "unidirectional arrow" "->" ::unidirectional-rule
    "bidirectional arrow" "<->" ::bidirectional-rule))

(h/defrule- <string>
  (h/rep* (apply h/+ <alphanumeric-string>
                 (map h/lit [\- \_ \. \, \: \; \? \! \( \) \[ \] \{ \} \* \^ \+
                             \/ \& \% \$ \# \@ \| \\ \º \ª \· \| \< \> \~]))))

(h/defrule- <rule>
  "Consumes a Kappa rule."
  {:no-memoize? true}
  (h/for [name (h/opt (concat-nested-rules (h/circumfix (h/lit \') <string> (h/lit \')))),
          lhs (h/prefix (h/opt <ws>) <expr>)
          arrow (circumfix-ws <arrow>)
          rhs <expr>
          rate (h/prefix (circumfix-ws (h/lit \@)) <number>)
          second-rate (h/opt (h/prefix (circumfix-ws (h/lit \,)) <number>))]
    (if (= arrow ::unidirectional-rule)
      (lang/make-rule name lhs rhs rate)
      [(lang/make-rule name lhs rhs rate)
       (lang/make-rule (str name "-op") rhs lhs second-rate)])))

;;; System
(h/defrule- <init-line>
  "Consumes a initial expression line in a Kappa system."
  {:no-memoize? true}
  (h/hook (fn [e] {:init [e]})
          (h/prefix (h/phrase "%init: ") <expr>)))

(h/defrule- <obs-line>
  "Consumes an observable line in a Kappa system."
  {:no-memoize? true}
  (h/prefix (h/phrase "%obs: ")
            (h/for [var-name (h/opt (h/circumfix (h/lit \') <alphanumeric-string> (h/lit \')))
                    expr (h/opt <expr>)]
              (if (nil? expr)
                {:obs-rules [var-name]}
                {:obs-exprs [var-name] :vars [[var-name expr]]}))))

(h/defrule- <var-line>
  "Consumes a variable-declaration line in a Kappa system."
  {:no-memoize? true}
  (h/prefix (h/phrase "%var: ")
            (h/for [var-name (h/circumfix (h/lit \') <alphanumeric-string> (h/lit \'))
                    expr <expr>]
              {:var [[var-name expr]]})))

(defn- make-system [v]
  (let [[init-obs-and-var rules] ((juxt filter remove) map? v)
        {:keys [init obs-exprs obs-rules vars]} (-> (merge-with concat init-obs-and-var)
                                                    (update-in [:vars] (partial into {})))]
    (chamber/make-chamber (apply concat rules) (apply merge init) 1
                          (map vars obs-exprs) obs-rules)))

(h/defrule- <system>
  "Consumes a Kappa system specification."
  {:no-memoize? true}
  (h/hook make-system
          (h/separated-rep (circumfix-ws <line-break>)
                           (h/+ <rule> <init-line> <obs-line> <var-line>))))

;;; Parser fns... thanks do-template
(do-template [fn-name rule label]
  (defn fn-name [s]
    (h/match (h/make-state s) rule
      :success-fn (fn [product position] product)
      :failure-fn (fn [error] (throw (Exception. (str "Error parsing Kappa " label ":"
                                                      (h/format-parse-error error)))))))
  parse-expr <expr> "expression"
  parse-rule <rule> "rule"
  parse-system <system> "system")

;;; DSL macros
;; TODO how to do the parsing at compile-time?
(defmacro def-exprs [& bindings]
  (let [[vars exprs] (apply map vector (partition 2 bindings))]
    `(do ~@(map (fn [v e] `(def ~v (parse-expr ~e))) vars exprs))))

(defmacro let-exprs [bindings & body]
  (let [[locals exprs] (apply map list (partition 2 bindings))]
    `(let ~(vec (mapcat (fn [l e] `(~l (parse-expr ~e))) locals exprs))
       ~@body)))

(defmacro def-rules [& bindings]
  (let [[vars rules] (apply map list (partition 2 bindings))]
    `(do ~@(map (fn [v r-str]
                  (if (vector? v)
                    `(let [r# (parse-rule ~r-str)]
                       (def ~(v 0) (nth r# 0))
                       (def ~(v 1) (nth r# 1)))
                    `(def ~v (parse-rule ~r-str))))
                vars rules))))

(defmacro let-rules [bindings & body]
  (let [[locals rules] (apply map list (partition 2 bindings))]
    `(let ~(vec (mapcat (fn [v r-str]
                          (if (vector? v)
                            `[r# (parse-rule ~r-str)
                              ~(v 0) (nth r# 0)
                              ~(v 1) (nth r# 1)]
                            `[~v (parse-rule ~r-str)]))
                        locals rules))
       ~@body)))

