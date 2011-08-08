(ns clj-parsedate.seconds
  (:import [org.joda.time Duration])
  (:use clj-parsedate.numbers)
  (:use clj-parsedate.parse)
  (:use clj-peg.combinators)
  (:use clj-parsedate.util))

(defn mkunit [name alts]
  (mkscope
   (mkmemo 
    (mkret (mkalt (map mkstr alts))
           (fn [b c] name)))))

(def days (mkscope (mkmemo (mkunit :days ["days" "day" "d"]))))
(def hours (mkscope (mkmemo (mkunit :hours ["hours" "hour" "h"]))))
(def minutes (mkscope (mkmemo (mkunit :minutes ["minutes" "minute" "min" "m"]))))
(def seconds (mkscope (mkmemo (mkunit :seconds ["seconds" "second" "sec" "s"]))))
(def millis (mkscope (mkmemo (mkunit :millis ["milliseconds" "millisecond" "ms"]))))

(def timeunit (mkscope (mkmemo (mkalt [days hours minutes seconds millis]))))

(defn todur [b c]
  (case (:unit b)
        :days  (Duration/standardDays (:n b))
        :hours (Duration/standardHours (:n b))
        :minutes (Duration/standardMinutes (:n b))
        :seconds (Duration/standardSeconds (:n b))
        :millis (Duration. (:n b))))

(def dur (mkscope (mkmemo (mkret (mkseq [(mkbind number :n)
                                         w*
                                         (mkbind timeunit :unit)])
                                 todur))))

(def dur* (mkscope (mkmemo (mkret (mkseq [(mkbind #'dur :d)
                                          (mkzom (mkseq [w+ (mkbind #'dur* :d2)]))])
                                       (fn [b c]
                                         (if (:d2 b)
                                           (.plus (:d b) (:d2 b))
                                           (:d b)))))))

(def parseseconds- (mkfn #'dur*))

(defn parseseconds [st]
  (parseseconds- st))
