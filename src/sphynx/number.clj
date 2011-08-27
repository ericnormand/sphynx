(ns sphynx.number
  (:use sphynx.util)
  (:use squarepeg))

;; We are going to be forward-referencing all of the rules in this
;; file so that we can define them in reverse order. I'll just add
;; everything to a single declare form for tidiness. I've even
;; alphabetized them as a kind of table of contents.

(declare
 a
 billion
 COMMA
 decimal
 hundred
 hundreds
 integer
 less100
 less1000
 lessbill
 lessmill
 lesstrill
 million
 mult100
 mult1000
 multhun
 number
 number-sens
 overtrill
 overtrill
 parse-number
 parse-number-
 sep
 teens
 ten
 tens
 thousand
 trillion
 units
 zero
 )

;; we use these helpers to add and add and multiply

(defn- addem [x y]
  (fn [b c]
    (+ (b x 0) (b y 0))))

(defn- addmul [x y m]
  (fn [b c]
    (+ (* m (b x 0)) (b y 0))))



;; We will want to function to do the cleanup before we
;; parse. Specifically, we want to trim whitespace and make the string
;; lowercase, since case isn't really important to us.

(defn parse-number
  "Inputs a String and tries to parse it as a number (which is the
  return value). It ignores whitespace at the beginning and end. It is
  case insensitive and somewhat lenient with the format of the
  number. Otherwise, the entire must match. It throws an exception if
  it can't find a number.

  It accepts optional options, which are keywords and values passed to the context.

    :allow-commas   When true (default), commas are allowed in numbers
                    (like in \"3,000\"). Setting it to false disallows
                    commas. Useful for CSV files."
  
  [s & options]
  (parse-number- s (apply assoc {} :allow-commas true options)))

;; We wrap this in parse-number. Basically, it's a rule that ignores
;; whitespace at the beginning and end, but the other than that, the
;; entire string must match.

(defrule parse-number- [w* number w* end])

;; A number is defined
;; This rule is exported. Feel free to use it in your rules.
;; It is case-insensitive

(defrule number (mklower #'number-sens))

;; A case-sensitive number

(defrule number-sens
  zero
  hundreds
  overtrill
  decimal
  integer
  a)

;; one way to express a number is as multiples of one hundred, like
;; "twelve hundred fifty-two" or "18 hundred"

(defrule hundreds
  [{:mult multhun} sep "hundred" sep {:less100 less100}] #{(addmul :mult :less100 100)}
  [{:mult multhun} sep "hundred"]                        #{(addmul :mult :_ 100)})

;; when talking about hundreds, you can't say "fifty hundred". So we
;; want teens, tens with a unit ("fifty three hundred"), and we'll be
;; nice and allow digits as in "56 hundred". But those digits have to
;; play by the same rules. multhun is what's allowed before "hundred".
;; stuff like "three hundred" is handled with overtrill

(defrule multhun
  teens
  [{:tens tens} sep {:units units}] #{(addem :tens :units)}
  [{:num integer} (? [b c]
                     (and
                      (> (:num b) 10)
                      (< (:num b) 100)
                      (pos? (rem (:num b) 10))))])

;; we can also spell out numbers up to 999 trillion
;; this rule will recursively add billions, millions, etc., all the
;; way down to units

(defrule overtrill
  [{:a mult1000} sep "trillion" sep {:b lesstrill}] #{(addmul :a :b 1000000000000)}
  [{:a mult1000} sep "trillion"]                    #{(addmul :a :_ 1000000000000)}
  lesstrill)

;; match a number in long form less than a trillion

(defrule lesstrill
  [{:a mult1000} sep "billion" sep {:b lessbill}] #{(addmul :a :b 1000000000)}
  [{:a mult1000} sep "billion"]                   #{(addmul :a :_ 1000000000)}
  lessbill)

;; match a number in long form less than a billion

(defrule lessbill
  [{:a mult1000} sep "million" sep {:b lessmill}] #{(addmul :a :b 1000000)}
  [{:a mult1000} sep "million"]                   #{(addmul :a :b 1000000)}
  lessmill)

;; match a number in long form less than a million

(defrule lessmill
  [{:a mult1000} sep "thousand" sep {:b less1000}] #{(addmul :a :b 1000)}
  [{:a mult1000} sep "thousand"]                   #{(addmul :a :_ 1000)}
  less1000)

;; match a number in long form less than 1000

(defrule less1000
  [{:a mult100} sep "hundred" sep {:b less100}] #{(addmul :a :b 100)}
  [{:a mult100} sep "hundred"] #{(addmul :a :_ 100)}
  less100)

;; match a number in long form less than 100
;; the order here is important because we don't want to match for
;; instance "seven" on the front of "seventeen"

(defrule less100
  [{:tens tens} sep {:units units}] #{(addem :tens :units)}
  ten
  tens
  teens
  units)

;; match what's allowed before powers of 1000. Anything less than
;; 1000, a (for "a thousand"), or some digits less than 1000

(defrule mult1000
  less1000
  a
  [{:num integer} (? [b c] (< (:num b) 1000))])

;; match what's allowed before 100. Basically, anything less than 10.

(defrule mult100
  units
  a
  [{:num integer} (? [b c] (< (:num b) 10))])

;; we define all of the English words that are related to numbers

;; These are our units.

(defrule units
  "one"       #{1}
  "two"       #{2}
  "three"     #{3}
  "four"      #{4}
  "five"      #{5}
  "six"       #{6}
  "seven"     #{7}
  "eight"     #{8}
  "nine"      #{9})

;; another rule to refer to the teens

(defrule teens
  "eleven"    #{11}
  "twelve"    #{12}
  "thirteen"  #{13}
  "fourteen"  #{14}
  "fifteen"   #{15}
  "sixteen"   #{16}
  "seventeen" #{17}
  "eighteen"  #{18}
  "nineteen"  #{19})

;; multiples of ten below 100 (excluding 10 itself)

(defrule tens
  "twenty"    #{20}
  "thirty"    #{30}
  "forty"     #{40}
  "fifty"     #{50}
  "sixty"     #{60}
  "seventy"   #{70}
  "eighty"    #{80}
  "ninety"    #{90}
)

;; a/an is a synonym for one

(defrule a (or     "an" "a")   #{1})

(defrule zero      "zero"      #{0})

(defrule ten       "ten"       #{10})

(defrule hundred   "hundred"   #{100})
(defrule thousand  "thousand"  #{1000})
(defrule million   "million"   #{1000000})
(defrule billion   "billion"   #{1000000000})
(defrule trillion  "trillion"  #{1000000000000})

;; match an integer

(defrule integer
  [{(or "-" {% "+"}) ?} digit {[COMMA digit] *}] #{(fn [b c]
                                                     (Long/parseLong (seqtostring (:ret b))))})

;; match a number with decimal place. This one is kind of messy. We
;; should be able to get the whole string it matches.

(defrule decimal
  [{:a [{(or "-" {% "+"}) ?} digit {[COMMA digit] *}]}
   "."
   {:b {digit +}}] #{(fn [b c]
                       (Double/parseDouble (str (seqtostring (:a b)) "." (seqtostring (:b b)))))})

;; separator between words in long-form is either whitespace, -, or
;; and

(defrule sep
  [w* "-" w*]
  [w+ "and" w+]
  w+)

;; in parsing digits, we ignore commas, which are optional

(defrule COMMA {% {[(? [b c] (:allow-commas c)) ","] ?}})
