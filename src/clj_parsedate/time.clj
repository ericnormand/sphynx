(ns clj-parsedate.time
  (:import [org.joda.time LocalTime])
  (:use clj-parsedate.util)
  (:use clj-parsedate.parse)
  (:use clj-peg.combinators))

(def colon (mklit \:))
(def dot (mklit \.))

(def am (mkstr "am"))
(def pm (mkstr "pm"))

(def hour24 (mkseq [(mkbind integer :hour)   (mkpred #(-> % :hour (<= 24)))]))
(def hour12 (mkseq [(mkbind integer :hour)   (mkpred #(-> % :hour (<= 12)))]))
(def minute (mkseq [(mkbind integer :minute) (mkpred #(-> % :minute (< 60)))]))
(def second (mkseq [(mkbind integer :second) (mkpred #(-> % :second (< 60)))]))
(def millis  (mkseq [(mkbind integer :millis) (mkpred #(-> % :millis (< 1000)))]))

(def timestr (mkalt [(mkret (mkseq [hour12
                                 (mkopt (mkseq [colon minute
                                                (mkopt (mkseq [colon second
                                                               (mkopt (mkseq [dot millis]))]))])) w* am])
                         #(LocalTime. (% :hour) (% :minute 0) (% :second 0) (% :millis 0)))
                  (mkret (mkseq [hour12
                                 (mkopt (mkseq [colon minute
                                                (mkopt (mkseq [colon second
                                                               (mkopt (mkseq [dot millis]))]))])) w* pm])
                         #(LocalTime. (+ 12 (% :hour)) (% :minute 0) (% :second 0) (% :millis 0)))
                  (mkret (mkseq [hour24
                                 (mkopt (mkseq [colon minute
                                                (mkopt (mkseq [colon second
                                                               (mkopt (mkseq [dot millis]))]))]))])
                         #(LocalTime. (% :hour) (% :minute 0) (% :second 0) (% :millis 0)))]))

(def parsetime- (mkfn (mkseq [timestr end])))

(defn parsetime [s]
  (-> s
      (.trim)
      (.toLowerCase)
      parsetime-))
