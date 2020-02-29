(defproject riverford-poc "0.1.0-SNAPSHOT"
  :description "Riverford Search PoC"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [me.raynes/fs "1.4.6"]
                 [clucie "0.4.2"]
                 [cljcc "0.1.3"]]
  :repl-options {:init-ns riverford-poc.core})
