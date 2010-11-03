## Kapjure
### Capturing the combinatorial complexity of biological systems

Kapjure is an open-source <a href="http://clojure.org/">Clojure</a> library for simulating and analyzing
<a href="http://www.kappalanguage.org">Kappa</a> models.

#### License

The code is released under the <a href="http://www.gnu.org/licenses/gpl.html">GNU GPL</a>.

#### Downloading the Source Code

Just execute the following command at the terminal:

    git clone git://github.com/rhz/clj-kappa.git

To download clj-kappa's source code into a directory called `clj-kappa` under the current directory.

#### Using it

First, put the directory `clj-kappa/src` in your *Java CLASSPATH*.
Then, open a Clojure REPL and when you are at it:

    user=> (require '[kappa :as k])

This will make the library to be loaded under the handy alias `k`.

#### Basic tutorial

Do you want to create some Kappa rules? Then:

    (k/def-rules
      [r1 r1-op] "E(x), S(x) <-> E(x!1), S(x!1) @ 1"
      r2 "E(x!1), S(x!1) -> E(x), P @ 1")

This will define three vars: `r1`, `r1-op` and `r2`.
As you probably already noticed, reversible rules must be bound by a pair.
In this case, `r1` is bound to the rule going forwards and `r1-op` to the rule going backwards.

To create Kappa expressions is equally easy:

    (k/def-exprs
      e1 "E(x), S(x), S(x), S(x)"
      e2 "E(x!1), S(x!1), S(x), S(x)")

Both `def-rules` and `def-exprs` have their
<a href="http://clojure.org/special_forms#Special Forms--(let [bindings* ] exprs*)">let</a>
counterparts: `let-rules` and `let-exprs`.

### Contributors

 * Ricardo Honorato-Zimmer (`rhz` at GitHub and `rata_` at `#clojure` at FreeNode)

