(ns clj-parsedate.parsedatetime
  (:import [org.joda.time DateTime])
  (:use clj-peg.combinators)
  (:use clj-parsedate.parse)
  (:use clj-parsedate.time)
  (:use clj-parsedate.dates)
  (:use clj-parsedate.util))

(defn combine-date-time [d t]
  (DateTime.
   (.getYear d)
   (.getMonthOfYear d)
   (.getDayOfMonth d)
   (.getHourOfDay t)
   (.getMinuteOfHour t)
   (.getSecondOfMinute t)
   (.getMillisOfSecond t)))

(def datetime- (mkscope
                (mkmemo
                 (mkalt [(mkret (mkseq [(mkbind date :date) w+ (mkopt (mkseq [(mkstr "at") w+])) (mkbind timestr :time)])
                                (fn [b c] (combine-date-time (:date b) (:time b))))
                         (mkret (mkseq [(mkbind timestr :time) w+ (mkbind date :date)])
                                (fn [b c] (combine-date-time (:date b) (:time b))))
                         date
                         (mkret (mkbind timestr :time)
                                (fn [b c] (combine-date-time (:now c) (:time b))))]))))

(def datetime (mkfn datetime-))

(defn parsedatetime
  ([st] (if (nil? st)
          (DateTime.)
          (parsedatetime st (DateTime.))))
  ([st dt]
     (let [s (-> st
                 (.trim)
                 (.toLowerCase))]
       (if (= s "")
         (DateTime.)
         (datetime s {:now dt})))))
