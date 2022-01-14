(ns instant-runoff.core-test
  (:require [clojure.test :refer :all]
            [instant-runoff.core :refer :all]))

(def example "p1
1. curvy wide
2. minimalist
3. Phoenix

p2
1. curvy wide
2. minimalist
3. Phoenix

p3
1. Phoenix
2. minimalist
3. curvy wide

p4
1. curvy wide
2. Phoenix
3. minimalist

p5
1. minimalist
2. curvy wide
3. Phoenix

p6
1. Phoenix
2. minimalist
3. curvy wide")

(deftest simple-situation
  (testing "Simple situation"
    (is (= :curvy-wide (ffirst (instant-runoff (parse example)))))))
