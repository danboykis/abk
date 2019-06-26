(ns abk.core
  (:require [abk.dag :refer [graph-sort]]
            [clojure.spec.alpha :as s]))

(s/def ::blueprint (s/map-of keyword? ::state))
(s/def ::state (s/keys :req [::start] :opt [::stop ::deps]))
(s/def ::start ifn?)
(s/def ::stop  ifn?)
(s/def ::deps  (s/and (s/or :explicit-deps (s/coll-of keyword? :into #{})
                            :implicit-deps #(= ::everything %))
                      (s/conformer second)))


(defn- apply-exclusions [blueprint exclusions]
  (let [bp (apply dissoc blueprint exclusions)]
    (into bp (map (fn [[k {::keys [deps] :as v}]]
                    (if (empty? deps)
                      [k v]
                      [k (assoc v ::deps (apply disj deps exclusions))]))) bp)))

(defn validate [blueprint]
  (let [bp (s/conform ::blueprint blueprint)]
    (if (= ::s/invalid bp)
      (throw (ex-info "bad blueprint" (s/explain-data ::blueprint blueprint)))
      bp)))

(defn- expand-deps [m everything]
  (into (empty m) (map (fn [[k {::keys [deps] :as v}]]
                         [k (if (= everything deps)
                              (assoc v ::deps (disj (set (keys m)) k))
                              v)]))
        m))

(defn process-blueprint [blueprint exclusions]
  (-> blueprint
      validate
      (expand-deps ::everything)
      (apply-exclusions exclusions)))

(defn start-state! [{:keys [state-ref blueprint info warn] :or {info println warn println}} o]
  (info "starting: " o)
  (when (contains? blueprint o)
    (when-let [start-fn! (get-in blueprint [o ::start])]
      (try
        (let [started-state (start-fn! state-ref)]
          (swap! state-ref assoc o started-state))
        (catch Exception ex
          (warn ex (str "Failed to start: " o))
          (throw ex))))))

(defn start! [{:keys [state-ref blueprint info warn] :or {info println
                                                          warn println}
               :as m}
              & exclusions]

  (let [blueprint   (process-blueprint blueprint exclusions)
        start-order (reverse (graph-sort blueprint))]
    (doseq [o start-order]
      (start-state! (assoc m :blueprint blueprint) o))
    (keys @state-ref)))

(defn stop-state! [{:keys [state-ref blueprint info warn] :or {info println warn println}} o]
  (let [s @state-ref]
    (info "stopping: " o)
    (when (contains? blueprint o)
      (when-let [stop-fn! (get-in blueprint [o ::stop])]
        (try
          (stop-fn! (get s o))
          (info "---\t" o)
          (swap! state-ref dissoc o)
          (catch Exception ex
            (warn ex (str "Failed to stop: " o))
            (throw ex)))))))


(defn stop! [{:keys [blueprint info warn] :or {info println
                                               warn println}
              :as m}
             & exclusions]
  (let [blueprint  (process-blueprint blueprint exclusions)
        stop-order (graph-sort blueprint)]
    (doseq [o stop-order]
      (stop-state! (assoc m :blueprint blueprint) o))))

(defn- depends-on? [blueprint state s]
  (let [deps (get-in blueprint [s ::deps])]
    (cond
      (= state s) true
      (contains? deps state) true
      :else (boolean (some (partial depends-on? blueprint state) deps)))))

(defn restart-state! [{:keys [blueprint state state-ref info warn] :or {info println
                                                              warn           println}
                       :as   m}
                      & exclusions]

  (let [blueprint (process-blueprint blueprint exclusions)
        potential-stop-states (into []
                                    (take-while (complement #{state}))
                                    (graph-sort blueprint))

        stop-states (filterv (partial depends-on? blueprint state) (conj potential-stop-states state))
        stripped-blueprint (assoc m :blueprint (select-keys blueprint stop-states)
                                    :state-ref state-ref)]
    (stop! stripped-blueprint)
    (start! stripped-blueprint)))
