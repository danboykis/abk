(defproject com.danboykis/abk "0.1.3-SNAPSHOT"
  :description "One atom state management for clojure apps"
  :url "http://github.com/danboykis/abk"
  :license {:name "Unlicense"
            :url "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :deploy-repositories [["releases" {:url "https://repo.clojars.org" :sign-releases false}]
                        ["snapshots" :clojars]])
