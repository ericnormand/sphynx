(ns sphynx.test.number
  (:use sphynx.number)
  (:use clojure.test))

(def smallnumbers ["Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"])
     
(def tennumbers ["", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"])
     
(def scalenumbers (reverse ["", "Thousand", "Million", "Billion", "Trillion"]))

(defn threedigittowords [n]
  (let [hundreds (long (/ n 100))
        tu (rem n 100)
        tens (long (/ tu 10))
        units (rem tu 10)]
    (str (if (pos? hundreds)
           (str (nth smallnumbers hundreds) " Hundred" (if (pos? tu) " and " ""))
           "")

         (if (>= tens 2)
           (str (nth tennumbers tens) (if (pos? units) (str " " (nth smallnumbers units)) ""))
           (if (pos? tu)
             (nth smallnumbers tu)
             "")))))

(defn multiplier [a m]
  (if (empty? a)
    ""
    (str a " " m)))

(defn gennumber [n]
  (if (zero? n)
    (nth smallnumbers 0)
    (let [groups (loop [g [] n n iter 5]
                   (if (> iter 0)
                     (recur (conj g (rem n 1000)) (long (/ n 1000)) (dec iter))
                     (reverse g)))]
      (.trim (apply str (interpose " " (map multiplier (map threedigittowords groups) scalenumbers)))))))

(defn randomnumbers [max]
  (lazy-seq (cons (long (rand max)) (randomnumbers max))))

(deftest testallnumbers
  (doseq [n (take 500 (randomnumbers 999999999999))]
    (is (= n (parse-number (gennumber n))))))

(deftest specifictests
  (dotimes [x 100]
   (is (= 500 (parse-number "5 hundred")))
   (is (= 20000 (parse-number "20 thousand")))
   (is (= 1000000001 (parse-number "one-billion and one")))
   (is (= 1200 (parse-number "12 hundred")))))

(deftest testdecimal
  (dotimes [x 100]
    (let [a (Math/random)]
      (is (= a (parse-number (str a))))))
  (is (= -1000 (parse-number "-1000")))
  (is (= 1000 (parse-number "+1000")))  
  (is (= 1000 (parse-number "1,000")))
  (is (is (thrown? Exception (parse-number "80,000" :allow-commas false))))
  (is (= [\,] (:i (number "80," {} {} {}))))
  (is (= 90434.45 (parse-number "+90,434.45")))
  (is (= -883434.23 (parse-number "-883,434.23")))
  )
