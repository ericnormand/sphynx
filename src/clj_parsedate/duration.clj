(ns clj-parsedate.duration
  (:import [org.joda.time Period])
  (:use clj-parsedate.numbers)
  (:use clj-parsedate.parse)
  (:use clj-peg.combinators)
  (:use clj-parsedate.util))

(defn mkunit [name alts]
  (mkscope (mkmemo 
            (mkbind (mkret (mkalt (map mkstr alts))
                           (constantly name))
                    (fn [b c] (:unit b))))))

(def days (mkscope (mkmemo (mkunit :days ["days" "day" "d"]))))
(def weeks (mkscope (mkmemo (mkunit :weeks ["weeks" "week" "w"]))))
(def months (mkscope (mkmemo (mkunit :months ["months" "month"]))))
(def years (mkscope (mkmemo (mkunit :years ["years" "year" "y"]))))
(def hours (mkscope (mkmemo (mkunit :hours ["hours" "hour" "h"]))))
(def minutes (mkscope (mkmemo (mkunit :minutes ["minutes" "minute" "min" "m"]))))
(def seconds (mkscope (mkmemo (mkunit :seconds ["seconds" "second" "sec" "s"]))))
(def millis (mkscope (mkmemo (mkunit :millis ["milliseconds" "millisecond" "ms"]))))

(def timeunit (mkscope (mkmemo (mkalt [days weeks months years hours minutes seconds millis]))))

(defn toduration [b c]
  (case (:unit b)
        :days  (Period. 0 0 0 (:n b) 0 0 0 0)
        :weeks (Period. 0 0 (:n b) 0 0 0 0 0)
        :months (Period. 0 (:n b) 0 0 0 0 0 0)
        :years (Period. (:n b) 0 0 0 0 0 0 0)
        :hours (Period. 0 0 0 0 (:n b) 0 0 0)
        :minutes (Period. 0 0 0 0 0 (:n b) 0 0)
        :seconds (Period. 0 0 0 0 0 0 (:n b) 0)
        :millis  (Period. 0 0 0 0 0 0 0 (:n b))))

(def duration (mkret (mkseq [(mkbind number :n)
                             w*
                             timeunit])
                     toduration))

(def parseduration- (mkfn duration))

(defn parseduration [st]
  (parseduration- st))
