(ns clj-parsedate.time
  (:import [org.joda.time LocalTime])
  (:use clj-parsedate.util)
  (:use clj-parsedate.parse)
  (:use clj-peg.combinators))

(def colon (mklit \:))
(def dot   (mklit \.))

(def am (mkscope (mkmemo (mkstr "am"))))
(def pm (mkscope (mkmemo (mkstr "pm"))))

(def hour24 (mkscope (mkmemo (mkseq [(mkbind integer :hour)   (mkpred (fn [b c] (-> b :hour (<= 24))))]))))
(def hour12 (mkscope (mkmemo (mkseq [(mkbind integer :hour)   (mkpred (fn [b c] (-> b :hour (<= 12))))]))))
(def minute (mkscope (mkmemo (mkseq [(mkbind integer :minute) (mkpred (fn [b c] (-> b :minute (< 60))))]))))
(def secon  (mkscope (mkmemo (mkseq [(mkbind integer :second) (mkpred (fn [b c] (-> b :second (< 60))))]))))
(def millis (mkscope (mkmemo (mkseq [(mkbind integer :millis) (mkpred (fn [b c] (-> b :millis (< 1000))))]))))

(def timestr (mkscope (mkmemo (mkalt [(mkret (mkseq [(mkbind hour12 :hour)
                                                     (mkopt
                                                      (mkseq
                                                       [colon
                                                        (mkbind minute :minute)
                                                        (mkopt
                                                         (mkseq
                                                          [colon
                                                           (mkbind secon :second)
                                                           (mkopt
                                                            (mkseq
                                                             [dot
                                                              (mkbind millis :millis)]))]))])) w* am])
                                             (fn [b c] (LocalTime. (b :hour) (b :minute 0) (b :second 0) (b :millis 0))))
                                      
                                      (mkret (mkseq [(mkbind hour12 :hour)
                                                     (mkopt
                                                      (mkseq
                                                       [colon
                                                        (mkbind minute :minute)
                                                        (mkopt
                                                         (mkseq
                                                          [colon
                                                           (mkbind secon :second)
                                                           (mkopt
                                                            (mkseq
                                                             [dot
                                                              (mkbind millis :millis)]))]))])) w* pm])
                                             (fn [b c] (LocalTime. (+ 12 (b :hour)) (b :minute 0) (b :second 0) (b :millis 0))))
                                      (mkret (mkseq [(mkbind hour24 :hour)
                                                     (mkopt
                                                      (mkseq
                                                       [colon
                                                        (mkbind minute :minute)
                                                        (mkopt
                                                         (mkseq
                                                          [colon
                                                           (mkbind secon :second)
                                                           (mkopt
                                                            (mkseq
                                                             [dot
                                                              (mkbind millis :millis)]))]))]))])
                                             (fn [b c] (LocalTime. (b :hour) (b :minute 0) (b :second 0) (b :millis 0))))]))))

(def parsetime- (mkfn (mkseq [timestr end])))

(defn parsetime [s]
  (-> s
      (.trim)
      (.toLowerCase)
      parsetime-))
