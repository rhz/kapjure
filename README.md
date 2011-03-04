## Kapjure
#### Capturing the combinatorial complexity of biological systems

Kapjure is an open-source <a href="http://clojure.org/">Clojure</a> library for simulating and analyzing
<a href="http://www.kappalanguage.org">Kappa</a> models.

#### License

The code is released under the <a href="http://www.gnu.org/licenses/gpl.html">GNU GPL</a>.

#### Downloading the Source Code

Just execute the following command at the terminal:

    git clone git://github.com/rhz/kapjure.git

To download kapjure's source code into a directory called `kapjure` under the current directory.

#### Using it

First, put the directory `kapjure/src` in your *Java CLASSPATH*.
Then, open a Clojure REPL and when you are at it:

    user=> (require '[kappa :as k])

This will make the library to be loaded under the handy alias `k`.

#### Basic tutorial

# Rules and expressions

Do you want to create some Kappa rules? Then:

    (k/def-rules
      [r1 r1-op] "E(x), S(x) <-> E(x!1), S(x!1) @ 1"
      r2 "E(x!1), S(x!1) -> E(x), P @ 0.1")

This will define three vars: `r1`, `r1-op` and `r2`.
As you probably already noticed, reversible rules must be bound by a pair.
In this case, `r1` is bound to the rule going forwards and `r1-op` to the rule going backwards.

To create Kappa expressions is equally easy:

    (k/def-exprs
      e1 "E(x), E(x), S(x), S(x), S(x), S(x)"
      e2 "E(x), E(x!1), S(x!1), S(x), S(x), S(x)"
      obs1 "P(x)", obs2 "S(x)")

Both `def-rules` and `def-exprs` have their
<a href="http://clojure.org/special_forms#Special Forms--(let [bindings* ] exprs*)">let</a>
counterparts: `let-rules` and `let-exprs`.

# Simulation

Once you have some rules and a expression, you can create a *reaction chamber*.
A chamber is a compartment where a simulation occurs.
To create a chamber you use the function `make-chamber` and to generate one of the possible
new states the chamber can transform into you use `gen-event`.
New states are generated based on the modified stochastic simulation algorithm (Gillespie's algorithm)
that appears in this <a href="http://www.springerlink.com/content/k6202r6207358424/">paper</a>.

    (let [initial-chamber (k/make-chamber [r1 r1-op r2] e1 [obs1 obs2] [])]
      (take 10 (iterate k/gen-event initial-chamber)))

Here we use Clojure's
<a href="http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/iterate">iterate</a>
function to generate an infinite sequence of states from `initial-chamber`.
Each state is the result of applying `gen-event` to the last generated state.
Then, <a href="http://clojure.github.com/clojure/clojure.core-api.html#clojure.core/take">take</a>
takes just the first 10 states from that sequence.
So that's the result of calling the above code: the first 100 states (including the initial one) of
running a simulation with the specified system.

One important remark: Kapjure's data structure, as Clojure's ones, are *immutable*.
That means every change you make over any of them generates a new copy with the modification instead.
This is worth noting because so you can use the whole Clojure core and standard library with them,
as we have done with `iterate` and `take` in the previous code.

After you have a simulation, you can ask what was the solution and the time at each generated state:

    (let [initial-chamber (k/make-chamber [r1 r1-op r2] e1 [obs1 obs2] [])
          simulation (take 10 (iterate k/gen-event initial-chamber))]
      (doseq [[n step] (map vector (iterate inc 0) simulation)]
        (println "Iteration" n "=> time:" (:time step) ", solution:" (:mixture step))))

As the initial solution just comprises 6 agents, there's no problem printing the solution.
But as solutions get larger, you probably wouldn't like to print them for each step to see
how the simulation is going.
Instead, what you want is to track the counts of some complexes.
That's what the third argument to `make-chamber` is for!
For example, in `initial-chamber` we're tracking the complexes S(x) and P(x),
that is, the substrate and the product of that Michaelian enzymatic reaction.
If you want to get a map for their counts, just call `get-sim-counts`:

    (let [initial-chamber (k/make-chamber [r1 r1-op r2] e1 [obs1 obs2] [])
          simulation (take 10 (iterate k/gen-event initial-chamber))]
      (k/get-sim-counts simulation))

# Plotting

This could be easily plotted using <a href="http://incanter.org/">Incanter</a>.
In fact, the function `plot-obs-exprs` in `kappa.graphics` will make this plot for you.
To call it, you must pass it the simulation (a sequence of chambers) and optionally a title:

    (require '[kappa.graphics :as g])
    (let [initial-chamber (k/make-chamber [r1 r1-op r2] e1 [obs1 obs2] [])
          simulation (take 10 (iterate k/gen-event initial-chamber))]
      (g/view (g/plot-obs-exprs simulation :title "Michaelian enzyme")))

The `view` and `save` functions in `kappa.graphics` do exactly the same as their
homonymous functions in `incanter.core`.
These two functions have been exposed through `kappa.graphics` so you don't
have to import Incanter as well.

# Reading Kappa files

To read Kappa files (`.ka`), you can use the `slurp` function in `clojure.core` and
`k/parse-system`.

    (require '[kappa.io :as io])
    (let [initial-chamber (k/parse-system (slurp "filename.kappa"))]
      (io/simulate-write-and-report "filename.txt" initial-chamber 100000))


### Contributors

 * Ricardo Honorato-Zimmer (`rhz` at GitHub and `rata_` at `#clojure` at FreeNode)
 * Felipe Nuñez (`fnunez` at GitHub)

