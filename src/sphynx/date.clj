(ns sphynx.date
  (:import [java.text DateFormatSymbols])
  (:import [org.joda.time Instant DateTime Period])
  (:use sphynx.duration)
  (:use sphynx.util)
  (:use squarepeg))

;; We are going to be forward-referencing all of the rules in this
;; file so that we can define them in reverse order. I'll just add
;; everything to a single declare form for tidiness. I've even
;; alphabetized them as a kind of table of contents.

(declare
 abstime
 date
 date-sens
 dayname
 futuredayname
 parse-date
 parse-date-
 pastdayname
 relative-time
 )

(defn lastday [dt d]
  (if (> (.getDayOfWeek dt) d)
    (.withDayOfWeek dt d)
    (-> dt (.minusWeeks 1) (.withDayOfWeek d))))

(defn nextday [dt d]
  (if (> (.getDayOfWeek dt) d)
    (.withDayOfWeek dt d)
    (-> dt (.plusWeeks 1) (.withDayOfWeek d))))

(defn parse-date
  "Inputs a String and tries to parse it as a date (which is the
  return value). It ignores whitespace at the beginning and end. It is
  case insensitive and somewhat lenient with the format of the
  date. Otherwise, the entire string must match. It throws an
  exception if it can't find a date.

  It accepts options, which are keywords passed to the
  context.

    At the moment, we support setting :now to a DateTime which will be
    used instead of the current time."

  [st & options]
  (parse-date- st (apply assoc {} :now (DateTime.) options)))

(defrule parse-date- [w* date w* end])

;; We export this rule. Feel free to use it in your own rules.
;; date requires that :now be set in the context to a DateTime

(defrule date (mklower #'date-sens))

(defrule date-sens
  relative-time
  abstime)

(defrule relative-time
  [{:dur duration} w+ "from" w+ {:abs abstime}] #{(fn [b c] (-> b :abs (.plus (:dur b))))}
  [{:dur duration} w+ "ago"]                    #{(fn [b c] (-> (:now c) (.minus (:dur b))))}
  ["in" w+ {:dur duration}]                     #{(fn [b c] (-> (:now c) (.plus (:dur b))))})

(defrule abstime
  "today"     #{(fn [b c] (:now c))}
  "now"       #{(fn [b c] (:now c))}
  "yesterday" #{(fn [b c] (-> c :now (.minusDays 1)))}
  "tomorrow"  #{(fn [b c] (-> c :now (.plusDays 1)))}
  futuredayname
  pastdayname)

(defrule futuredayname
  [(or "this" "next") w+ {:day dayname}] #{(fn [b c]
                                             (nextday (:now c) (:day b)))})

(defrule pastdayname
  ["last" w+ {:day dayname}] #{(fn [b c]
                                 (lastday (:now c) (:day b)))})

(defrule dayname
  "monday"    #{1}
  "tuesday"   #{2}
  "wednesday" #{3}
  "thursday"  #{4}
  "friday"    #{5}
  "saturday"  #{6}
  "sunday"    #{7})

(def monthmap {
               "january" 1
               "february" 2
               "march" 3
               "april" 4
               "may" 5
               "june" 6
               "july" 7
               "august" 8
               "september" 9
               "october" 10
               "november" 11
               "december" 12
               })

