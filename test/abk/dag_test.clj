(ns abk.dag-test
  (:require [clojure.test :refer :all]
            [abk.dag :refer :all]
            [clojure.set :as s]))

(def graph {:5  {:abk.core/deps [:11]}
            :7  {:abk.core/deps [:11 :8]}
            :11 {:abk.core/deps [:2 :9 :10]}
            :8  {:abk.core/deps [:9]}
            :3  {:abk.core/deps [:8 :10]}
            :2  {}
            :9  {}
            :10 {}})

(def easy-dag {:1 {:abk.core/deps [:2]}
               :2 {:abk.core/deps [:3]}
               :3 {:abk.core/deps [:4]}})

(defn idx-of [e xs]
  (ffirst (filter (fn [[_ x]] (= e x)) (map-indexed vector xs))))

(defn test-sorted [g sorted]
  (loop [[head & tail] sorted]
    (if (nil? head)
      ::sorted
      (if (every? #(< (idx-of head sorted) (idx-of % sorted)) (-> (get g head) :abk.core/deps))
        (recur tail)
        (throw (ex-info (str "sorted: " sorted ", started: " head "\nbad sort at: " head) g))))))

(deftest sort-dag
  (testing "sort dag"
    (is (= ::sorted (test-sorted graph (graph-sort graph))))))

(deftest sort-easy
  (testing "sort easy dag"
    (is (= (graph-sort easy-dag) [:1 :2 :3 :4]))))
