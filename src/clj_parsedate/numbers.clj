(ns clj-parsedate.numbers
  (:use clj-parsedate.util)
  (:use clj-parsedate.parse)
  (:use clj-peg.combinators))

(def sep (mkalt [(mkseq [w* (mkstr "-") w*])
                 w+
                 (mkseq [w+ (mkstr "and") w+])]))

(def a         (mkret (mkstr "a")         (constantly 1)))

(def zero      (mkret (mkstr "zero")      (constantly 0)))

(def one       (mkret (mkstr "one")       (constantly 1)))
(def two       (mkret (mkstr "two")       (constantly 2)))
(def three     (mkret (mkstr "three")     (constantly 3)))
(def four      (mkret (mkstr "four")      (constantly 4)))
(def five      (mkret (mkstr "five")      (constantly 5)))
(def six       (mkret (mkstr "six")       (constantly 6)))
(def seven     (mkret (mkstr "seven")     (constantly 7)))
(def eight     (mkret (mkstr "eight")     (constantly 8)))
(def nine      (mkret (mkstr "nine")      (constantly 9)))
(def ten       (mkret (mkstr "ten")       (constantly 10)))

(def eleven    (mkret (mkstr "eleven")    (constantly 11)))
(def twelve    (mkret (mkstr "twelve")    (constantly 12)))
(def thirteen  (mkret (mkstr "thirteen")  (constantly 13)))
(def fourteen  (mkret (mkstr "fourteen")  (constantly 14)))
(def fifteen   (mkret (mkstr "fifteen")   (constantly 15)))
(def sixteen   (mkret (mkstr "sixteen")   (constantly 16)))
(def seventeen (mkret (mkstr "seventeen") (constantly 17)))
(def eighteen  (mkret (mkstr "eighteen")  (constantly 18)))
(def nineteen  (mkret (mkstr "nineteen")  (constantly 19)))

(def twenty    (mkret (mkstr "twenty")    (constantly 20)))
(def thirty    (mkret (mkstr "thirty")    (constantly 30)))
(def forty     (mkret (mkstr "forty")     (constantly 40)))
(def fifty     (mkret (mkstr "fifty")     (constantly 50)))
(def sixty     (mkret (mkstr "sixty")     (constantly 60)))
(def seventy   (mkret (mkstr "seventy")   (constantly 70)))
(def eighty    (mkret (mkstr "eighty")    (constantly 80)))
(def ninety    (mkret (mkstr "ninety")    (constantly 90)))

(def hundred   (mkret (mkstr "hundred")   (constantly 100)))
(def thousand  (mkret (mkstr "thousand")  (constantly 1000)))
(def million   (mkret (mkstr "million")   (constantly 1000000)))
(def billion   (mkret (mkstr "billion")   (constantly 1000000000)))
(def trillion  (mkret (mkstr "trillion")  (constantly 1000000000000)))

(def units (mkalt [one
                   two
                   three
                   four
                   five
                   six
                   seven
                   eight
                   nine]))

(def teens (mkalt [eleven
                   twelve
                   thirteen
                   fourteen
                   fifteen
                   sixteen
                   seventeen
                   eighteen
                   nineteen]))

(def tens (mkalt [twenty
                  thirty
                  forty
                  fifty
                  sixty
                  seventy
                  eighty
                  ninety]))

(defn addem [x y]
  (fn [b]
    (+ (b x 0) (b y 0))))

(defn addmul [x y m]
  (fn [b]
    (+ (* m (b x 0)) (b y 0))))

(def less100 (mkalt [(mkret (mkseq [(mkbind tens :tens) sep (mkbind units :units)]) (addem :tens :units))
                     tens
                     teens
                     units]))

(def mult100 (mkalt [units
                     a]))

(def less1000 (mkalt [(mkret (mkseq [(mkbind mult100 :mult100) sep (mkstr "hundred") sep (mkbind less100 :less100)])
                             (addmul :mult100 :less100 100))
                      (mkret (mkseq [(mkbind mult100 :mult100) sep (mkstr "hundred")])
                             (addmul :mult100 :_ 100))
                      less100]))

(def mult1000 (mkalt [less1000
                      a]))

(def lessmill (mkalt [(mkret (mkseq [(mkbind mult1000 :less1000x) sep (mkstr "thousand") sep (mkbind less1000 :less1000a)])
                             (addmul :less1000x :less1000a 1000))
                      (mkret (mkseq [(mkbind mult1000 :less1000) sep (mkstr "thousand")])
                             (addmul :less1000x :less1000a 1000))
                      less1000]))

(def lessbill (mkalt [(mkret (mkseq [(mkbind mult1000 :less1000y) sep (mkstr "million") sep (mkbind lessmill :lessmill)])
                             (addmul :less1000y :lessmill 1000000))
                      (mkret (mkseq [(mkbind mult1000 :less1000y) sep (mkstr "million")])
                             (addmul :less1000y :lessmill 1000000))
                      lessmill]))

(def lesstrill (mkalt [(mkret (mkseq [(mkbind mult1000 :less1000z) sep (mkstr "billion") sep (mkbind lessbill :lessbill)])
                             (addmul :less1000z :lessbill 1000000000))
                      (mkret (mkseq [(mkbind mult1000 :less1000z) sep (mkstr "billion")])
                             (addmul :less1000z :lessbill 1000000000))
                      lessbill]))

(def overtrill (mkalt [(mkret (mkseq [(mkbind mult1000 :less1000d) sep (mkstr "trillion") sep (mkbind lesstrill :lesstrill)])
                             (addmul :less1000d :lesstrill 1000000000000))
                      (mkret (mkseq [(mkbind mult1000 :less1000d) sep (mkstr "trillion")])
                             (addmul :less1000d :lesstrill 1000000000000))
                      lesstrill]))

(def ten-unit (mkret (mkseq [(mkbind tens :tens) sep (mkbind units :units)]) (addem :tens :units)))

(def multhun (mkalt [teens
                     ten-unit]))

(def hundreds (mkalt [(mkret (mkseq [(mkbind multhun :hmulthun) sep (mkstr "hundred") sep (mkbind less100 :hless100)])
                             (addmul :hmulthun :hless100 100))
                      (mkret (mkseq [(mkbind multhun :hmulthun) sep (mkstr "hundred")])
                             (addmul :hmulthun :hless100 100))]))

(def number (mkalt [zero
                           hundreds
                           overtrill
                           integer
                           a]))

(def parse-number- (mkfn (mkseq [number end])))

(defn parse-number [s]
  (-> s
      (.trim)
      (.toLowerCase)
      parse-number-))
