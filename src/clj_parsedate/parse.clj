(ns clj-parsedate.parse
  (:use clj-parsedate.util)
  (:use clj-peg.combinators))

(defn seqtostring [st] (apply str st))

(def integer (mkret (mk1om digit) (fn [b c] (-> b :ret seqtostring readint))))

(def w+ (mknothing (mk1om whitespace)))
(def w* (mknothing (mkzom whitespace)))

