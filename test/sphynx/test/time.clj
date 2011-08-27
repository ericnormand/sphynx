(ns sphynx.test.time
  (:use clojure.test)
  (:use sphynx.time)
  (:import [org.joda.time LocalTime]))

(deftest testsometimes
  (let [lt19 (LocalTime. 19 43 23 90)]
    (is (= lt19 (parse-time "7:43:23.090pm")))
    (is (= lt19 (parse-time "19:43:23.090")))
    (is (= lt19 (parse-time "19:43:23.09"))))
  (let [eight (LocalTime. 8 0 0 0)
        twenty (LocalTime. 20 0 0 0)]
    (is (= eight (parse-time "eight o'clock")))
    (is (= eight (parse-time "eight")))
    (is (= eight (parse-time "eight am")))
    (is (= eight (parse-time "Eight")))
    (is (= twenty (parse-time "eight pm"))))
  ;; corner cases
  (let [noon (LocalTime. 12 0 0 0)
        midnight (LocalTime. 0 0 0 0 )]
    (is (= noon (parse-time "twelve o'clock")))
    (is (= noon (parse-time "noon")))
    (is (= noon (parse-time "twelve noon")))
    (is (= midnight (parse-time "midnight")))
    (is (= midnight (parse-time "twelve midnight")))
    (is (= midnight (parse-time "twelve am")))
    (is (= noon (parse-time "twelve pm")))
    (is (= noon (parse-time "12:00pm")))
    (is (= midnight (parse-time "12:00:00am"))))
  ;; predex
  (let [t330pm (LocalTime. 15 30 0 0)
        t415pm (LocalTime. 16 15 0 0)
        t810am (LocalTime. 8 10 0 0)
        t945pm (LocalTime. 21 45 0 0)]
    (is (= t330pm (parse-time "3:30pm")))
    (is (= t330pm (parse-time "half past three pm")))
    (is (= t415pm (parse-time "quarter past four pm")))
    (is (= t810am (parse-time "ten past eight")))
    (is (= t945pm (parse-time "quarter to ten pm")))
    )
  (let [t330pm (LocalTime. 15 30 0 0)
        t415pm (LocalTime. 16 15 0 0)
        t810am (LocalTime. 8 10 0 0)
        t945pm (LocalTime. 21 45 0 0)]
    (is (= t330pm (parse-time "three thirty pm")))
    (is (= t415pm (parse-time "four fifteen pm")))
    (is (= t810am (parse-time "eight ten")))
    )
  )

;; three thirty
;; quarter to

