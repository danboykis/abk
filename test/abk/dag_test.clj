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

(defn test-sorted [m sorted]
  (loop [[head & tail] sorted
         started #{}]
    (if (nil? head)
      ::sorted
      (let [s (-> m (get head) :abk.core/deps)]
        (if (or (nil? s) (every? started s))
          (recur tail (conj started head))
          (throw (ex-info (str "map: " m ", started: " started "\nbad sort at: " s " ," sorted) {})))))))


(deftest sort-dag
  (testing "sort dag"
    (is (= ::sorted (test-sorted graph (graph-sort graph))))))

(deftest sort-easy
  (testing "sort easy dag"
    (is (= (graph-sort easy-dag) (reverse [:1 :2 :3 :4])))))
