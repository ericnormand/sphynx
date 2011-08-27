(ns sphynx.seconds
  (:import [org.joda.time Duration])
  (:use sphynx.number)
  (:use sphynx.util)
  (:use squarepeg))

(declare
 days
 dur
 dur*
 hours
 millis
 minutes
 parse-seconds
 parse-seconds-
 seconds
 timeunit
 )

(defn parse-seconds [st & options]
  (parse-seconds- st))

(defrule parse-seconds- [w* dur* w* end])

(defrule days    (or "days" "day" "d")                  #{(constantly :days)})
(defrule hours   (or "hours" "hour" "h")                #{(constantly :hours)})
(defrule minutes (or "minutes" "minute" "min" "m")      #{(constantly :minutes)})
(defrule seconds (or "seconds" "second" "sec" "s")      #{(constantly :seconds)})
(defrule millis  (or "milliseconds" "millisecond" "ms") #{(constantly :millis)})

(defrule timeunit
  days
  hours
  minutes
  seconds
  millis)

(defn todur [b c]
  (case (:unit b)
    :days    (Duration/standardDays    (:n b))
    :hours   (Duration/standardHours   (:n b))
    :minutes (Duration/standardMinutes (:n b))
    :seconds (Duration/standardSeconds (:n b))
    :millis  (Duration.                (:n b))))

(defrule dur
  [{:n number} w* {:unit timeunit}] #{todur})

(defrule dur*
  [{:d dur} {[w+ {:d2 dur*}] *}] #{(fn [b c]
                                     (if (:d2 b)
                                       (.plus (:d b) (:d2 b))
                                       (:d b)))})
