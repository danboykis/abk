(ns abk.core-test
  (:require [clojure.test :refer :all]
            [abk.core :as abk])
  (:import [clojure.lang ExceptionInfo]))

(def blueprint {:state/one {::abk/start (fn [_] (println "[start] state/one") 1)
                            ::abk/stop (fn [x] (println "[stop] state/one -> " x))}

                :state/two {::abk/start (fn [s] (println "[start] state/two -> " @s) 2)
                            ::abk/stop (fn [x] (println "[stop] state/two -> " x)) ::abk/deps [:state/one]}

                :state/three {::abk/start (fn [s] (println "[start] state/three -> " @s) 3)
                              ::abk/stop (fn [x] (println "[stop] state/three -> " x)) ::abk/deps [:state/two]}

                :state/four {::abk/start (fn [s] (println "[start] state/four -> " @s) 4)
                             ::abk/stop (fn [x] (println "[stop] state/four -> " x))}})


(deftest start-states
  (testing "starting blueprint"
    (let [s (atom nil)]
      (is (= (keys blueprint) (keys @(abk/start! {:blueprint blueprint :state-ref s}))))
      (is (= (:state/one   @s) 1))
      (is (= (:state/two   @s) 2))
      (is (= (:state/three @s) 3))
      (is (= (:state/four  @s) 4)))))

(deftest start-one
  (testing "starting one in blueprint"
    (let [s (atom nil)
          _ (abk/start-one! {:blueprint blueprint :state :state/four :state-ref s})]
      (is (= (set (keys @s)) #{:state/four}))
      (is (= (:state/four @s) 4)))))

(deftest start-one-complex
  (testing "starting one complex in blueprint"
    (let [s (atom nil)
          _ (abk/start-one! {:blueprint blueprint :state :state/three :state-ref s})]
      (is (= (set (keys @s)) #{:state/three :state/two :state/one}))
      (is (= (:state/three @s) 3))
      (is (= (:state/two   @s) 2))
      (is (= (:state/one   @s) 1)))))

(deftest restart-state
  (testing "restart-state in blueprint"
    (let [s (atom nil)
          _ (abk/start-one! {:blueprint blueprint :state :state/three :state-ref s})]
      (is (= (set (keys @s)) #{:state/three :state/two :state/one}))
      (is (= (:state/three @s) 3))
      (abk/restart-state! {:blueprint blueprint :state :state/three :state-ref s}))))

(def blueprint2 {:state/one {::abk/start (fn [_] (println "[start] state/one") 1)
                             ::abk/stop (fn [x] (println "[stop] state/one -> " x))
                             ::abk/deps [:state/does-not-exist]}})

(deftest unreferenced-state
  (testing "fail on unreferenced dep"
    (try
      (let [s (atom {})
            _ (abk/start! {:blueprint blueprint2 :state-ref s})])
      (catch ExceptionInfo ei
        (is (= (ex-data ei) {:state/one {::abk/deps #{:state/does-not-exist}}}))))))

