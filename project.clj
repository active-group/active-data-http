(defproject de.active-group/active-data-http "0.2.0"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.11.0"]
                 [de.active-group/active-data-translate "0.1.0"]
                 [de.active-group/active-clojure "0.43.0"]
                 [com.cognitect/transit-clj "1.0.333"]
                 [com.cognitect/transit-cljs "0.8.280"]
                 [cljs-ajax "0.7.5" :scope "provided"]
                 [metosin/reitit-core "0.7.2" :scope "provided"]
                 [metosin/reitit-ring "0.7.2" :scope "provided"]
                 [metosin/reitit-spec "0.7.2" :scope "provided"]
                 [de.active-group/reacl-c-basics "0.11.8" :scope "provided"]]

  :plugins [[lein-codox "0.10.8"]]

  ;; run clojurescript tests via
  ;; > npm run test

  :profiles {:cljs-test
             {:source-paths ["src" "test"]
              :dependencies [[thheller/shadow-cljs "2.27.1"]]}
             :test
             {:dependencies []}
             :dev
             {:dependencies [[clj-kondo "2024.09.27"]]}}

  :codox {:metadata {:doc/format :markdown}
          :src-dir-uri "http://github.com/active-group/active-monad/blob/master/"
          :src-linenum-anchor-prefix "L"})
