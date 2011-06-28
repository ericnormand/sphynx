(ns clj-parsedate.numbers
  (:use clj-parsedate.util)
  (:use clj-parsedate.parse)
  (:use clj-peg.combinators))

(def sep (mkalt [(mkseq [w* (mkstr "-") w*])
                 w+
                 (mkseq [w+ (mkstr "and") w+])]))

(def zero (mkret (mkstr "zero") (constantly 0)))

(def less-than-10-map
  {
   "a"            1
   "one"          1
   "two"          2
   "three"        3
   "four"         4
   "five"         5
   "six"          6
   "seven"        7
   "eight"        8
   "nine"         9
   })

(def less10 (mkalt (map (fn [[w n]] (mkret (mkstr w) (constantly n))) less-than-10-map)))

(def teens-map
  { 
   "ten"          10
   "eleven"       11
   "twelve"       12
   "thirteen"     13
   "fourteen"     14
   "fifteen"      15
   "sixteen"      16
   "seventeen"    17
   "eighteen"     18
   "nineteen"     19
   "ninteen"      19 ;; Common mis-spelling
   })

(def teens (mkalt (map (fn [[w n]] (mkret (mkstr w) (constantly n))) teens-map)))

(def ordinals
  { 
   "first"    1
   "second"   2
   "third"    3
   "fourth"   4
   "fifth"    5
   "sixth"    6
   "seventh"  7
   "eighth"   8
   "ninth"    9
   "tenth"    10
   })



(def tens-map
  { 
   "twenty"   20
   "thirty"   30
   "forty"    40
   "fourty"   40 ;; Common mis-spelling
   "fifty"    50
   "sixty"    60
   "seventy"  70
   "eighty"   80
   "ninety"   90
   })

(def tens (mkalt (map (fn [[w n]] (mkret (mkstr w) (constantly n))) tens-map)))
(def ten-unit (mkalt [(mkret (mkseq [(mkbind tens :ten) sep (mkbind less10 :unit)]) #(+ (:ten %) (:unit %)))
                      tens
                      teens
                      less10]))

(def hundreds (mkalt [(mkret (mkseq [(mkbind less10 :hu) sep (mkstr "hundred") sep (mkbind ten-unit :unit)]) #(+ (:unit %) (* (:hu %) 100) ))
                      (mkret (mkseq [(mkbind less10 :hu) sep (mkstr "hundred")]) #(* (:hu %) 100) )
                      ten-unit]))

(def funny-thousands (mkret (mkseq [(mkbind ten-unit :u) sep (mkstr "hundred")])
                            #(* (:u %) 100)))

(def thousands (mkret (mkseq [(mkbind hundreds :u) sep (mkstr "thousand")])
                      #(* (:u %) 1000)))

(def millions (mkret (mkseq [(mkbind hundreds :u) sep (mkstr "million")])
                     #(* (:u %) 1000000)))

(def billions (mkret (mkseq [(mkbind hundreds :u) sep (mkstr "billion")])
                     #(* (:u %) 1000000000)))

(def trillions (mkret (mkseq [(mkbind hundreds :u) sep (mkstr "trillion")])
                      #(* (:u %) 1000000000000)))

(def thousands-or-less (mkalt [(mkret (mkseq [(mkbind thousands :thou) sep (mkbind hundreds :hun)]) #(+ (:thou %) (:hun %)))
                               thousands
                               hundreds]))

(def millions-or-less (mkalt [(mkret (mkseq [(mkbind millions :mil) sep (mkbind thousands-or-less :thou)])
                                     #(+ (:thou %) (:mil %)))
                              millions
                              thousands-or-less]))

(def billions-or-less (mkalt [(mkret (mkseq [(mkbind billions :bil) sep (mkbind millions-or-less :mil)])
                                     #(+ (:bil %) (:mil %)))
                              billions
                              millions-or-less]))

(def trillions-or-less (mkalt [(mkret (mkseq [(mkbind trillions :tril) sep (mkbind billions-or-less :bil)])
                                      #(+ (:tril %) (:bil %)))
                               trillions
                               billions-or-less]))

(def longnumber trillions-or-less)

(def parse-number- (mkalt [funny-thousands
                           zero
                           longnumber
                           integer
                           ]))

(def parse-number (mkfn (mkseq [parse-number- end])))
