(defproject cocoa "0.1.0-SNAPSHOT"
  :description "cocoa"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.671"]
                 [org.clojure/core.async "0.3.465"]
                 [cljsjs/react-modal "2.3.2-0"]
                 [reagent "0.7.0"]
                 [bidi "2.1.2"]
                 [cljs-ajax "0.7.3"]]
  :jvm-opts ^:replace ["-Xmx1g" "-server"]
  :plugins [[lein-npm "0.6.2"]
            [lein-cljsbuild "1.1.7"]]
  :npm {:dependencies [[source-map-support "0.4.0"]]}
  :target-path "target"
  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src/cljs/cocoa/"]
              :compiler {:output-to "resources/compiled/cljs/bundle.js"
                         :output-dir "resources/compiled/cljs/out"
                         :asset-path "/resources/compiled/cljs/out"
                         :optimizations :none
                         :main cocoa.presenter.browser.core
                         :pretty-print true}}
             {:id "release"
              :source-paths ["src/cljs/cocoa/"]
              :compiler {:output-to "resources/compiled/cljs/bundle.js"
                         :optimizations :advanced}}]})
