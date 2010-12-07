(defproject kapjure "0.1"
  :description "Clojure library for simulating and analyzing Kappa models (www.kappalanguage.org)"
  :url "http://github.com/rhz/kapjure"
  :license {:name "GNU General Public License v3.0"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :tasks [protobuf.tasks]
  :dependencies [[clojure "1.2.0"]
                 [clojure-contrib "1.2.0"]
                 [org.clojars.ohpauleez/fnparse "3.0.0alpha4"]
                 [clojure.data.finger-tree "0.0.1-SNAPSHOT"]
                 [incanter "1.2.3"]]
  :aot [kappa.isomerization kappa.simple-models]
  :warn-on-reflection true)

