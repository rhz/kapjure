(ns kappa
  {:doc ""
   :author "Ricardo Honorato-Zimmer"})

;; taken from compojure
(defn immigrate
  "Create a public var in this namespace for each public var in the
  namespaces named by ns-names. The created vars have the same name, value,
  and metadata as the original except that their :ns metadata value is this
  namespace."
  [& ns-names]
  (doseq [ns ns-names]
    (require ns :reload-all)
    (doseq [[sym var] (ns-publics ns)]
      (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
        (if (.isBound var)
          (intern *ns* sym (var-get var))
          (intern *ns* sym))))))

(apply immigrate '[kappa.language kappa.parser kappa.chamber kappa.maps])

(comment ;; taken from potemkin
(defmacro import-fn 
  "Given a function in another namespace, defines a function by
   the same name in the current namespace.  Argument lists and
   doc-strings are preserved."
  [sym]
  (let [m (meta (eval sym))
        m (meta (intern (:ns m) (:name m)))
        n (:name m)
        arglists (:arglists m)
        doc (:doc m)]
    (list `def (with-meta n {:doc doc :arglists (list 'quote arglists)}) (eval sym))))

(require 'kappa.language)
(import-fn kappa.language/create-rule)
)

