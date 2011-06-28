(ns clj-parsedate.dates
  (:import [java.text DateFormatSymbols])
  (:import [org.joda.time Instant DateTime Period])
  (:use clj-parsedate.parse)
  (:use clj-parsedate.duration)
  (:use clj-peg.combinators)
  (:use clj-parsedate.util))

(def today (mkret (mkstr "today") (fn [b] (DateTime.))))
(def now   (mkret (mkstr "now") (fn [b] (DateTime.))))
(def yesterday (mkret (mkstr "yesterday") (fn [b] (-> (DateTime.) (.minusDays 1)))))
(def tomorrow (mkret (mkstr "tomorrow") (fn [b] (-> (DateTime.) (.plusDays 1)))))

(def daymap {"monday"    1
             "tuesday"   2
             "wednesday" 3
             "thursday"  4
             "friday"    5
             "saturday"  6
             "sunday"    7})

(def dayname (mkbind (mkalt (map (fn [[d n]] (mkret (mkstr d) (constantly n))) daymap)) :day))

(defn lastday [dt d]
  (if (> (.getDayOfWeek dt) d)
    (.withDayOfWeek dt d)
    (-> dt (.minusWeeks 1) (.withDayOfWeek d))))

(defn nextday [dt d]
  (if (> (.getDayOfWeek dt) d)
    (.withDayOfWeek dt d)
    (-> dt (.plusWeeks 1) (.withDayOfWeek d))))

(def futuredayname (mkret (mkseq [(mkalt [(mkstr "this")
                                          (mkstr "next")
                                          (mkpred #(= :future (:tense %)))])
                                  w+
                                  dayname])
                          #(nextday (DateTime.) (:day %))))

(def pastdayname (mkret (mkseq [(mkalt [(mkstr "last")
                                        (mkpred #(= :past (:tense %)))])
                                w+
                                dayname])
                        #(lastday (DateTime.) (:day %))))

(def abstime (mkalt [today now yesterday tomorrow futuredayname pastdayname]))

(def relative-time (mkalt [(mkret (mkseq [(mkbind duration :dur) w+ (mkstr "from") w+ (mkbind abstime :abs)])
                                  (fn [b] (-> b :abs (.plus (:dur b)))))
                           (mkret (mkseq [(mkbind duration :dur) w+ (mkstr "ago")])
                                  (fn [b] (-> (DateTime.) (.minus (:dur b)))))
                           (mkret (mkseq [(mkstr "in") w+ (mkbind duration :dur)])
                                  (fn [b] (-> (DateTime.) (.plus (:dur b)))))]))

(def date (mkalt [relative-time
                  abstime]))

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

