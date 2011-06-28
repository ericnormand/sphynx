(ns clj-parsedate.duration
  (:import [org.joda.time Period])
  (:use clj-parsedate.parse)
  (:use clj-peg.combinators)
  (:use clj-parsedate.util))

(def numberb integer)

(defn mkunit [name alts]
  (mkbind (mkret (mkalt (map mkstr alts))
                 (constantly name))
          :unit))

(def days (mkunit :days ["days" "day" "d"]))
(def weeks (mkunit :weeks ["weeks" "week" "w"]))
(def months (mkunit :months ["months" "month"]))
(def years (mkunit :years ["years" "year" "y"]))
(def hours (mkunit :hours ["hours" "hour" "h"]))
(def minutes (mkunit :minutes ["minutes" "minute" "min" "m"]))
(def seconds (mkunit :seconds ["seconds" "second" "sec" "s"]))
(def millis (mkunit :millis ["milliseconds" "millisecond" "ms"]))

(def timeunit (mkalt [days weeks months years hours minutes seconds millis]))

(defn toduration [b]
  (case (:unit b)
        :days  (Period. 0 0 0 (:n b) 0 0 0 0)
        :weeks (Period. 0 0 (:n b) 0 0 0 0 0)
        :months (Period. 0 (:n b) 0 0 0 0 0 0)
        :years (Period. (:n b) 0 0 0 0 0 0 0)
        :hours (Period. 0 0 0 0 (:n b) 0 0 0)
        :minutes (Period. 0 0 0 0 0 (:n b) 0 0)
        :seconds (Period. 0 0 0 0 0 0 (:n b) 0)
        :millis  (Period. 0 0 0 0 0 0 0 (:n b))))

(def duration (mkret (mkseq [(mkbind numberb :n)
                             w*
                             timeunit])
                     toduration))
