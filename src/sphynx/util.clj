(ns sphynx.util
  (:use squarepeg))

;; A utility to make a string from a seq of characters

(defn seqtostring
  "Make a String from a seq of characters."
  [st]
  (apply str st))

;; We want some shortcuts for matching and ignoring whitespace.

(defrule w+ {% {whitespace +}})
(defrule w* {% {whitespace *}})

