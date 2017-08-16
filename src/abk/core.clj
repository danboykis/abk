(ns abk.core
  (:require [abk.dag :refer [graph-sort]]
            [clojure.set :as cs]
            [clojure.spec.alpha :as s]))

(s/def ::state (s/keys :req [::start] :opt [::stop ::deps]))
(s/def ::start ifn?)
(s/def ::stop ifn?)
(s/def ::deps (s/coll-of keyword?))

(s/def ::states (s/map-of keyword? ::state))

(s/def ::graph (s/keys :req [::states]))

(defn- check-args [graph]
  (when-not (s/valid? ::graph graph)
    (throw (ex-info "not a valid graph" (s/explain-data ::graph graph)))))

(defn- add-started [s dep]
  (update s ::started (fnil conj []) dep))

(defn- remove-started [s dep]
  (update s ::started #(vec (remove #{dep} %))))

(defn- run-state! [start-fn! state k]
  (try (start-fn! state k) (catch Exception e {::error e ::partial-state state})))

(defn start! [s {:keys [::states] :as g}]
  (check-args g)
  (let [deps (graph-sort states)]
    (assert (cs/subset? (set deps) (set (keys states))) "state-map doesn't have all the necessary states present in dep-graph")
    (loop [ss s
           [head-dep & tails-deps] deps]
      (if-not (nil? head-dep)
        (let [{start! ::start} (get states head-dep)
              started-state (run-state! start! ss head-dep)]
          (if (::error started-state)
            started-state
            (recur (-> (assoc ss head-dep started-state)
                       (add-started head-dep))
                   tails-deps)))
        ss))))

(defn stop! [s {:keys [::states] :as g}]
  (check-args g)
  (let [deps (reverse (graph-sort states))]
    (assert (cs/subset? (set deps) (set (keys states))) "state-map doesn't have all the necessary states present in dep-graph")
    (loop [ss s
           [head-dep & tails-deps] deps]
      (if-not (nil? head-dep)
        (let [{stop! ::stop} (get states head-dep)]
          (recur (do (stop! (get ss head-dep))
                     (-> (dissoc ss head-dep)
                         (remove-started head-dep)))
                 tails-deps))
        ss))))
