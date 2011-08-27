(ns sphynx.duration
  (:import [org.joda.time Period])
  (:use sphynx.number)
  (:use sphynx.util)
  (:use squarepeg))

;; We are going to be forward-referencing all of the rules in this
;; file so that we can define them in reverse order. I'll just add
;; everything to a single declare form for tidiness. I've even
;; alphabetized them as a kind of table of contents.

(declare
 duration
 duration*
 parse-duration
 parse-duration-
 timeunit
 timeunit-sens
)

(defn parse-duration
  "Inputs a String and tries to parse it as a duration (which is the
  return value). It ignores whitespace at the beginning and end. It is
  case insensitive and somewhat lenient with the format of the
  duration. Otherwise, the entire string must match. It throws an
  exception if it can't find a duration.

  It accepts options, which are keywords passed to the
  context.

    No options are currently supported."

  [st & options]
  (parse-duration- st))

(defrule parse-duration- [w* duration* w* end])

(defrule duration*
  [{:d duration} {[w+ {:d2 duration*}] *}] #{(fn [b c]
                                               (if (:d2 b)
                                                 (.plus (:d b) (:d2 b))
                                                 (:d b)))})

(defrule duration
  [{:n number} w* {:unit timeunit}] #{(fn [b c]
                                        (case (:unit b)
                                          :days    (Period. 0 0 0 (:n b) 0 0 0 0)
                                          :weeks   (Period. 0 0 (:n b) 0 0 0 0 0)
                                          :months  (Period. 0 (:n b) 0 0 0 0 0 0)
                                          :years   (Period. (:n b) 0 0 0 0 0 0 0)
                                          :hours   (Period. 0 0 0 0 (:n b) 0 0 0)
                                          :minutes (Period. 0 0 0 0 0 (:n b) 0 0)
                                          :seconds (Period. 0 0 0 0 0 0 (:n b) 0)
                                          :millis  (Period. 0 0 0 0 0 0 0 (:n b))))})

;; ms must come before m

(defrule timeunit (mklower #'timeunit-sens))

(defrule timeunit-sens
  (or "days" "day" "d")                  #{(constantly :days)}
  (or "weeks" "week" "w")                #{(constantly :weeks)}
  (or "months" "month")                  #{(constantly :months)}
  (or "years" "year" "y")                #{(constantly :years)}
  (or "hours" "hour" "h")                #{(constantly :hours)}
  (or "milliseconds" "millisecond" "ms") #{(constantly :millis)}
  (or "minutes" "minute" "min" "m")      #{(constantly :minutes)}
  (or "seconds" "second" "sec" "s")      #{(constantly :seconds)})
