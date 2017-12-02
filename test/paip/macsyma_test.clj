(ns paip.macsyma-test
  (:require [clojure.test :refer :all]
            [paip.macsyma :refer :all]))

(deftest infix->prefix-simple
  (testing "macsyma-simple"
    (is
      (= '(+ 2 3)
         (infix->prefix '(2 + 3))))))
