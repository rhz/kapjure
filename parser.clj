(ns clj-kappa.parser
  (:use name.choi.joshua.fnparse
        clojure.contrib.error-kit))

(deferror parse-error [] [state message message-args]
  {:msg (str "Parser error: "
             (apply format message message-args))
   :unhandled (throw-msg Exception)})

(def apply-str
  (partial apply str))

;; TODO write EBNF for each rule as documentation
;; Strings
(def alpha-num-char (lit-alt-seq "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
(def alpha-num-string (rep+ alpha-num-char))

;; Whitespace
(def space (lit \space))
(def tab (lit \tab))
(def newline-lit (lit \newline))
(def return-lit (lit \return))
(def line-break (rep+ (alt newline-lit return-lit)))
(def comment-lit (complex [_ (lit \#)
                           content (rep* (alt alpha-num-string (lit \space)))
                           _ (opt line-break)]
                          [:comment content]))
(def ws (constant-semantics (rep+ (alt space tab line-break comment-lit)) :ws))

;; Numbers
(def digit (lit-alt-seq "0123456789"))
(def whole-number (rep+ digit))
(def fractional-part (conc (lit \.) (rep* digit)))
(def exponential-sign (lit-alt-seq "eE"))
(def exponential-part (conc exponential-sign
                            (opt (alt (lit \+) (lit \-)))
                            whole-number))
(def decimal-number
     (complex [minus (opt (lit \-))
               above-one whole-number
               below-one (opt fractional-part)
               power (opt exponential-part)]
              (-> [minus above-one below-one power] flatten apply-str read-string)))
(def ratio (complex [minus (opt (lit \-))
                     numerator whole-number
                     div (lit \/)
                     denominator whole-number]
                    (-> [minus numerator div denominator] flatten apply-str read-string)))
(def number (alt ratio decimal-number))

;; Interface
(def begin-interface (constant-semantics (conc (opt ws) (lit \() (opt ws))
                                         ;; this keyword is a dummy value, it
                                         ;; doesn't appear in the final result.
                                         :begin-interface))
(def end-interface (constant-semantics (conc (opt ws) (lit \)) (opt ws))
                                       :end-interface)) ; the same for this
(defn separator [chr]
  (constant-semantics (conc (opt ws) (lit chr) (opt ws))
                      :separator)) ; and for this

(def internal-state (complex [i-s (opt (conc (lit \~) alpha-num-string))]
                             (if (nil? i-s) :any-state
                                 (apply-str (second i-s)))))
(def binding-state (complex [b-s (opt (conc (lit \!)
                                            (alt (constant-semantics (lit \?) :unspecified)
                                                 (constant-semantics (lit \_) :semi-link)
                                                 whole-number)))]
                            (let [b-s (second b-s)]
                              (cond (nil? b-s) :free
                                    (keyword? b-s) b-s
                                    :else (apply-str b-s)))))
(def site (complex [name alpha-num-string
                    i-s internal-state
                    b-s binding-state]
                   [(apply-str name) i-s b-s]))
(def site-list (rep* (invisi-conc site (opt (separator \,)))))
(def interface (complex [begin-iface (opt begin-interface)
                         iface (opt site-list)
                         end-iface (opt end-interface)]
                        (if (or (nil? begin-iface) (nil? iface) (nil? end-iface))
                          :empty-interface
                          iface)))

;; Agent
(def kappa-agent (complex [modifier (opt (invisi-conc whole-number (separator \*)))
                           ;;sep (conc (opt ws) (opt (lit \*)) (opt ws))
                           name alpha-num-string
                           _ (opt ws)
                           iface interface]
                          [(if (nil? modifier) 1
                               (read-string (apply-str modifier)))
                           (apply-str name) iface]))

;; Expression
(def expression (rep* (invisi-conc kappa-agent (opt (separator \,)))))

;; Rule
(def arrow (complex [_ (opt ws)
                     rule-type (alt (constant-semantics (lit-conc-seq "<->") :bidirectional-rule)
                                    (constant-semantics (lit-conc-seq "->") :unidirectional-rule))
                     _ (opt ws)]
                    rule-type))
(def rate-expr (complex [_ (conc (opt ws) (lit \@) (opt ws))
                         rate number]
                        rate))
(def rule-name (complex [rn (opt (conc (lit \') alpha-num-string (lit \')))]
                        (if (nil? rn) :unnamed-rule
                            (second rn))))
(def rule (complex [rn (opt rule-name)
                    lhs expression
                    rule-type arrow
                    rhs expression
                    rate rate-expr]
                   {:name rn :lhs lhs :rule-type rule-type :rhs rhs :rate rate}))

;; Rule set
(def rule-set (rep* (invisi-conc rule line-break)))

;; System
(def init-line (complex [_ (lit-conc-seq "%init:")
                         _ (rep* ws)
                         expr expression]
                        {:init expr}))
(def init-set (rep* (invisi-conc init-line line-break)))

(def obs-line (complex [_ (lit-conc-seq "%obs:")
                        _ (rep* ws)
                        obs (complex [var-name (opt (conc (lit \') alpha-num-string (lit \')))
                                      expr (opt expression)]
                                     (if (nil? expr)
                                       {:obs var-name} ; we want to observe a rule
                                       {:name var-name :obs expr}))] obs)) ; or an expression
(def obs-set (rep* (invisi-conc obs-line line-break)))

(def variable-line (complex [_ (lit-conc-seq "%var:")
                             _ (rep* ws)
                             var-name (conc (lit \') alpha-num-string (lit \'))
                             expr expression]
                            {:var var-name :value expr}))
(def variable-set (rep* (invisi-conc variable-line line-break)))

;; TODO causal flow analysis and perturbations

(def system (conc rule-set init-set (opt obs-set) (opt variable-set)))

;; Parse
(defn parse-rule [rule-str]
  (rule-match rule
              ;; failure fn
              #(raise parse-error %1 "invalid expression \"%s\""
                      [(apply str (:remainder %1))])
              ;; incomplete fn
              #(raise parse-error %2 "leftover data after a valid expression \"%s\""
                      [(apply str (:remainder %2))])
              ;; initial state
              {:remainder (seq rule-str)}))

(defn parse-expression [expr-str]
  (rule-match expression
              ;; failure fn
              #(raise parse-error %1 "invalid expression \"%s\""
                      [(apply str (:remainder %1))])
              ;; incomplete fn
              #(raise parse-error %2 "leftover data after a valid expression \"%s\""
                      [(apply str (:remainder %2))])
              ;; initial state
              {:remainder (seq expr-str)}))

(defn parse-agent [agent-str]
  (rule-match kappa-agent
              ;; failure fn
              #(raise parse-error %1 "invalid agent \"%s\""
                      [(apply str (:remainder %1))])
              ;; incomplete fn
              #(raise parse-error %2 "leftover data after a valid agent \"%s\""
                      [(apply str (:remainder %2))])
              ;; initial state
              {:remainder (seq agent-str)}))
