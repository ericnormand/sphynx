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

(def datetime- (mkscope (mkalt [(mkret (mkseq [(mkbind date :date) w+ (mkopt (mkseq [(mkstr "at") w+])) (mkbind timestr :time)]) #(combine-date-time (:date %) (:time %)))
                                (mkret (mkseq [(mkbind timestr :time) w+ (mkbind date :date)]) #(combine-date-time (:date %) (:time %)))
                                date
                                (mkret (mkbind timestr :time) #(.toDateTimeToday (:time %)))])))

(def datetime (mkfn datetime-))

(defn parsedatetime
  ([st] (if (nil? st)
          (DateTime.)
          (parsedatetime st (DateTime.))))
  ([st dt]
     (-> st
         (.trim)
         (.toLowerCase)
         (datetime {:now dt}))))
