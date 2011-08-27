(ns sphynx.test.duration
  (:use sphynx.duration)
  (:use clojure.test)
  (:import [org.joda.time Period]))

(deftest test-duration
  (is (= (Period/seconds 1) (parse-duration "1 second")))
  (is (= (.plus (Period/seconds 1)
                (Period/minutes 2)) (parse-duration "2 minutes 1 second"))))
