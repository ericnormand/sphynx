(ns clj-parsedate.numbers
  (:use clj-parsedate.util)
  (:use clj-parsedate.parse)
  (:use clj-peg.combinators))

(def sep (mkscope (mkmemo (mkalt [(mkseq [w* (mkstr "-") w*])
                                  (mkseq [w+ (mkstr "and") w+])
                                  w+
                                  ]))))

(def a         (mkscope (mkmemo (mkret (mkstr "a")         (constantly 1)))))

(def zero      (mkscope (mkmemo (mkret (mkstr "zero")      (constantly 0)))))

(def one       (mkscope (mkmemo (mkret (mkstr "one")       (constantly 1)))))
(def two       (mkscope (mkmemo (mkret (mkstr "two")       (constantly 2)))))
(def three     (mkscope (mkmemo (mkret (mkstr "three")     (constantly 3)))))
(def four      (mkscope (mkmemo (mkret (mkstr "four")      (constantly 4)))))
(def five      (mkscope (mkmemo (mkret (mkstr "five")      (constantly 5)))))
(def six       (mkscope (mkmemo (mkret (mkstr "six")       (constantly 6)))))
(def seven     (mkscope (mkmemo (mkret (mkstr "seven")     (constantly 7)))))
(def eight     (mkscope (mkmemo (mkret (mkstr "eight")     (constantly 8)))))
(def nine      (mkscope (mkmemo (mkret (mkstr "nine")      (constantly 9)))))
(def ten       (mkscope (mkmemo (mkret (mkstr "ten")       (constantly 10)))))

(def eleven    (mkscope (mkmemo (mkret (mkstr "eleven")    (constantly 11)))))
(def twelve    (mkscope (mkmemo (mkret (mkstr "twelve")    (constantly 12)))))
(def thirteen  (mkscope (mkmemo (mkret (mkstr "thirteen")  (constantly 13)))))
(def fourteen  (mkscope (mkmemo (mkret (mkstr "fourteen")  (constantly 14)))))
(def fifteen   (mkscope (mkmemo (mkret (mkstr "fifteen")   (constantly 15)))))
(def sixteen   (mkscope (mkmemo (mkret (mkstr "sixteen")   (constantly 16)))))
(def seventeen (mkscope (mkmemo (mkret (mkstr "seventeen") (constantly 17)))))
(def eighteen  (mkscope (mkmemo (mkret (mkstr "eighteen")  (constantly 18)))))
(def nineteen  (mkscope (mkmemo (mkret (mkstr "nineteen")  (constantly 19)))))

(def twenty    (mkscope (mkmemo (mkret (mkstr "twenty")    (constantly 20)))))
(def thirty    (mkscope (mkmemo (mkret (mkstr "thirty")    (constantly 30)))))
(def forty     (mkscope (mkmemo (mkret (mkstr "forty")     (constantly 40)))))
(def fifty     (mkscope (mkmemo (mkret (mkstr "fifty")     (constantly 50)))))
(def sixty     (mkscope (mkmemo (mkret (mkstr "sixty")     (constantly 60)))))
(def seventy   (mkscope (mkmemo (mkret (mkstr "seventy")   (constantly 70)))))
(def eighty    (mkscope (mkmemo (mkret (mkstr "eighty")    (constantly 80)))))
(def ninety    (mkscope (mkmemo (mkret (mkstr "ninety")    (constantly 90)))))

(def hundred   (mkscope (mkmemo (mkret (mkstr "hundred")   (constantly 100)))))
(def thousand  (mkscope (mkmemo (mkret (mkstr "thousand")  (constantly 1000)))))
(def million   (mkscope (mkmemo (mkret (mkstr "million")   (constantly 1000000)))))
(def billion   (mkscope (mkmemo (mkret (mkstr "billion")   (constantly 1000000000)))))
(def trillion  (mkscope (mkmemo (mkret (mkstr "trillion")  (constantly 1000000000000)))))

(def units (mkscope (mkmemo (mkalt [one
                                    two
                                    three
                                    four
                                    five
                                    six
                                    seven
                                    eight
                                    nine]))))

(def teens (mkscope (mkmemo (mkalt [eleven
                                    twelve
                                    thirteen
                                    fourteen
                                    fifteen
                                    sixteen
                                    seventeen
                                    eighteen
                                    nineteen]))))

(def tens (mkscope (mkmemo (mkalt [twenty
                                   thirty
                                   forty
                                   fifty
                                   sixty
                                   seventy
                                   eighty
                                   ninety]))))

(defn addem [x y]
  (fn [b c]
    (+ (b x 0) (b y 0))))

(defn addmul [x y m]
  (fn [b c]
    (+ (* m (b x 0)) (b y 0))))

(def less100 (mkscope (mkmemo (mkalt [(mkret (mkseq [(mkbind tens :tens) sep (mkbind units :units)])
                                             (addem :tens :units))
                                      ten
                                      tens
                                      teens
                                      units]))))

(def mult100 (mkscope (mkmemo (mkalt [units
                                      a
                                      (mkseq [(mkbind integer :num) (mkpred (fn [b c] (< (:num b) 10)))])
                                      ]))))

(def less1000 (mkscope (mkmemo (mkalt [(mkret (mkseq [(mkbind mult100 :a) sep (mkstr "hundred") sep (mkbind less100 :b)])
                                              (addmul :a :b 100))
                                       (mkret (mkseq [(mkbind mult100 :a) sep (mkstr "hundred")])
                                              (addmul :a :_ 100))
                                       less100]))))

(def mult1000 (mkscope (mkmemo (mkalt [less1000
                                       a
                                       (mkseq [(mkbind integer :num) (mkpred (fn [b c] (< (:num b) 1000)))])
                                       ]))))

(def lessmill (mkscope (mkmemo (mkalt [(mkret (mkseq [(mkbind mult1000 :a) sep (mkstr "thousand") sep (mkbind less1000 :b)])
                                              (addmul :a :b 1000))
                                       (mkret (mkseq [(mkbind mult1000 :a) sep (mkstr "thousand")])
                                              (addmul :a :_ 1000))
                                       less1000]))))

(def lessbill (mkscope (mkmemo (mkalt [(mkret (mkseq [(mkbind mult1000 :a) sep (mkstr "million") sep (mkbind lessmill :b)])
                                              (addmul :a :b 1000000))
                                       (mkret (mkseq [(mkbind mult1000 :a) sep (mkstr "million")])
                                              (addmul :a :b 1000000))
                                       lessmill]))))

(def lesstrill (mkscope (mkmemo (mkalt [(mkret (mkseq [(mkbind mult1000 :a) sep (mkstr "billion") sep (mkbind lessbill :b)])
                                               (addmul :a :b 1000000000))
                                        (mkret (mkseq [(mkbind mult1000 :a) sep (mkstr "billion")])
                                               (addmul :a :_ 1000000000))
                                        lessbill]))))

(def overtrill (mkscope (mkmemo (mkalt [(mkret (mkseq [(mkbind mult1000 :a) sep (mkstr "trillion") sep (mkbind lesstrill :b)])
                                               (addmul :a :b 1000000000000))
                                        (mkret (mkseq [(mkbind mult1000 :a) sep (mkstr "trillion")])
                                               (addmul :a :_ 1000000000000))
                                        lesstrill]))))

(def ten-unit (mkscope (mkmemo (mkret (mkseq [(mkbind tens :tens) sep (mkbind units :units)])
                                      (addem :tens :units)))))

(def multhun (mkscope (mkmemo (mkalt [teens
                                      ten-unit
                                      (mkseq [(mkbind integer :num) (mkpred (fn [b c] (and
                                                                                      (> (:num b) 10)
                                                                                      (< (:num b) 100)
                                                                                      (pos? (rem (:num b) 10)))))])
                                      ]))))

(def hundreds (mkscope (mkmemo (mkalt [(mkret (mkseq [(mkbind multhun :hmulthun) sep (mkstr "hundred") sep (mkbind less100 :hless100)])
                                              (addmul :hmulthun :hless100 100))
                                       (mkret (mkseq [(mkbind multhun :hmulthun) sep (mkstr "hundred")])
                                              (addmul :hmulthun :hless100 100))]))))

(def number (mkscope (mkmemo (mkalt [zero
                                     hundreds
                                     overtrill
                                     integer
                                     a]))))

(def parse-number- (mkfn number))

(defn parse-number [s]
  (-> s
      (.trim)
      (.toLowerCase)
      parse-number-))
