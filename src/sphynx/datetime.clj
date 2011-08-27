(ns sphynx.datetime
  (:import [org.joda.time DateTime])
  (:use squarepeg)
  (:use sphynx.time)
  (:use sphynx.date)
  (:use sphynx.util))

(declare
 datetime
 parse-datetime
 parse-datetime-
 )

(defn parse-datetime [s & options]
  (let [c (apply assoc {} :now (DateTime.) options)]
   (cond
    (nil? s)
    (:now c)
    (and (string? s) (empty? (.trim s)))
    (:now c)
    :otherwise
    (parse-datetime- s c))))

(defrule parse-datetime- [w* datetime w* end])

(defn combine-date-time [d t]
  (DateTime.
   (.getYear d)
   (.getMonthOfYear d)
   (.getDayOfMonth d)
   (.getHourOfDay t)
   (.getMinuteOfHour t)
   (.getSecondOfMinute t)
   (.getMillisOfSecond t)))

(defrule datetime
  [{:date date} w+ {["at" w+] ?} {:time timestr}] #{(fn [b c] (combine-date-time (:date b) (:time b)))}
  [{:time timestr} w+ {:date date}]               #{(fn [b c] (combine-date-time (:date b) (:time b)))}
  date
  {:time timestr}                                 #{(fn [b c] (combine-date-time (:now c) (:time b)))})
