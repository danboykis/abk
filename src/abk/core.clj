(ns abk.core
  (:require [abk.dag :refer [graph-sort]]
            [clojure.set :as cs]
            [clojure.spec.alpha :as s]))

(s/def ::states (s/map-of keyword? (s/and vector? #(= 2 (count %)) #(and (fn? (first %))
                                                                         (fn? (second %))))))

(s/def ::graph (s/keys :req [::dep-graph ::states]))
(s/def ::dep-graph (s/coll-of #(or (keyword? %) (vector? %))))

(defn- check-args [graph]
  (when-not (s/valid? ::graph graph)
    (throw (ex-info "not a valid graph" (s/explain-data ::graph graph)))))

(defn- add-started [s dep]
  (update s ::started (fnil conj []) dep))

(defn- remove-started [s dep]
  (update s ::started #(vec (remove #{dep} %))))

(defn- run-state! [start-fn! state k]
  (try (start-fn! state k) (catch Exception e {::error e ::partial-state state})))

(defn start! [s {:keys [::dep-graph ::states] :as g}]
  (check-args g)
  (let [deps (graph-sort dep-graph)]
    (assert (cs/subset? (set deps) (set (keys states))) "state-map doesn't have all the necessary states present in dep-graph")
    (loop [ss s
           [head-dep & tails-deps] deps]
      (if-not (nil? head-dep)
        (let [[start! _] (get states head-dep)
              started-state (run-state! start! ss head-dep)]
          (if (::error started-state)
            started-state
            (recur (-> (assoc ss head-dep started-state)
                       (add-started head-dep))
                   tails-deps)))
        ss))))

(defn stop! [s {:keys [::dep-graph ::states] :as g}]
  (check-args g)
  (let [deps (reverse (graph-sort dep-graph))]
    (assert (cs/subset? (set deps) (set (keys states))) "state-map doesn't have all the necessary states present in dep-graph")
    (loop [ss s
           [head-dep & tails-deps] deps]
      (if-not (nil? head-dep)
        (let [[_ stop!] (get states head-dep)]
          (recur (do (stop! (get ss head-dep))
                     (-> (dissoc ss head-dep)
                         (remove-started head-dep)))
                 tails-deps))
        ss))))
