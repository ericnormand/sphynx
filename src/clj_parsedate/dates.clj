(ns clj-parsedate.dates
  (:import [java.text DateFormatSymbols])
  (:import [org.joda.time Instant DateTime Period])
  (:use clj-parsedate.parse)
  (:use clj-parsedate.duration)
  (:use clj-peg.combinators)
  (:use clj-parsedate.util))

(def today     (mkscope (mkmemo (mkret (mkstr "today")     (fn [b c] (:now c))))))
(def now       (mkscope (mkmemo (mkret (mkstr "now")       (fn [b c] (:now c))))))
(def yesterday (mkscope (mkmemo (mkret (mkstr "yesterday") (fn [b c] (-> c :now (.minusDays 1)))))))
(def tomorrow  (mkscope (mkmemo (mkret (mkstr "tomorrow")  (fn [b c] (-> c :now (.plusDays 1)))))))

(def daymap {"monday"    1
             "tuesday"   2
             "wednesday" 3
             "thursday"  4
             "friday"    5
             "saturday"  6
             "sunday"    7})

(def dayname (mkscope (mkmemo (mkalt (map (fn [[d n]]
                                            (mkret (mkstr d) (fn [b c] n)))
                                          daymap)))))

(defn lastday [dt d]
  (if (> (.getDayOfWeek dt) d)
    (.withDayOfWeek dt d)
    (-> dt (.minusWeeks 1) (.withDayOfWeek d))))

(defn nextday [dt d]
  (if (> (.getDayOfWeek dt) d)
    (.withDayOfWeek dt d)
    (-> dt (.plusWeeks 1) (.withDayOfWeek d))))

(def futuredayname (mkscope (mkmemo (mkret (mkseq [(mkalt [(mkstr "this")
                                                           (mkstr "next")])
                                                   w+
                                                   (mkbind dayname :day)])
                                           (fn [b c]
                                             (nextday (:now c) (:day b)))))))

(def pastdayname (mkscope (mkmemo (mkret (mkseq [(mkstr "last")
                                                 w+
                                                 (mkbind dayname :day)])
                                         (fn [b c]
                                           (lastday (:now c) (:day b)))))))

(def abstime (mkscope (mkmemo (mkalt [today now yesterday tomorrow futuredayname pastdayname]))))

(def relative-time (mkscope (mkmemo (mkalt [(mkret (mkseq [(mkbind duration :dur) w+ (mkstr "from") w+ (mkbind abstime :abs)])
                                                   (fn [b c] (-> b :abs (.plus (:dur b)))))
                                            (mkret (mkseq [(mkbind duration :dur) w+ (mkstr "ago")])
                                                   (fn [b c] (-> (:now c) (.minus (:dur b)))))
                                            (mkret (mkseq [(mkstr "in") w+ (mkbind duration :dur)])
                                                   (fn [b c] (-> (:now c) (.plus (:dur b)))))]))))

(def date (mkscope (mkmemo (mkalt [relative-time
                                   abstime]))))

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

