(ns clj-parsedate.test.dates
  (:use clj-parsedate.duration)
  (:use clojure.test)
  (:import [org.joda.time Period]))

(deftest test-duration
  (is (= (Period/seconds 1) (parseduration "1 second")))
  (is (= (.plus (Period/seconds 1)
                (Period/minutes 2)) (parseduration "2 minutes 1 second"))))
