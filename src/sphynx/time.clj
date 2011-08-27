(ns sphynx.time
  (:import [org.joda.time LocalTime])
  (:use squarepeg)
  (:use sphynx.number)
  (:use sphynx.util))

(declare
 parse-time
 parse-time-
 colon
 dot
 am
 pm
 hour24
 hour12
 minute
 secon
 millis
 timestr
 oclock
 direct-time
 past
 tohr
 hour
 bound
 )

;; some utilities for using am/pm

(defn calc-am [b c]
  (let [hours (:hour b 0)
        minutes (:minute b 0)
        seconds (:second b 0)
        milliss (:millis b 0)
        ;; 12 o'clock am is really 0 in 24 hour time
        hours (if (= 12 hours)
                0
                hours)]
    (LocalTime. hours minutes seconds milliss)))

(defn calc-pm [b c]
  (let [hours (:hour b 0)
        minutes (:minute b 0)
        seconds (:second b 0)
        milliss (:millis b 0)
        ;; 12 o'clock pm is really 12 in 24 hour time
        hours (if (= 12 hours)
                12
                (+ 12 hours))]
    (LocalTime. hours minutes seconds milliss)))

(defn parse-time [s & options]
  (parse-time- s))

;; we can't use "time" because it's already taken

(defrule parse-time- [w* timestr w* end])

(defrule timestr
  direct-time
  hour
  )

(defrule hour
  [{["twelve" w+] ?} "noon"] #{(fn [b c] (LocalTime. 12 0 0 0))}
  [{["twelve" w+] ?} "midnight"] #{(fn [b c] (LocalTime. 0 0 0 0))}

  [{:hour hour12} w* am] #{calc-am}

  [{:hour hour12} w* pm] #{calc-pm}

  [{:hour number} w+ am] #{calc-am}
  [{:hour number} w+ pm] #{calc-pm}

  [{:hour number} {[w+ oclock] ?}] #{(fn [b c] (LocalTime. (:hour b) 0 0 0))}
  {:hour hour24} #{(fn [b c] (LocalTime. (b :hour 0) (b :minute 0) (b :second 0) (b :millis 0)))}
  )

(defrule direct-time
  [{:mins past} w+ {:hrs hour}] #{(fn [b c] (.plusMinutes (:hrs b) (:mins b)))}
  [{:mins tohr} w+ {:hrs hour}] #{(fn [b c] (.minusMinutes (:hrs b) (:mins b)))}

  [{:hour hour12} {[colon {:minute minute} {[colon {:second secon} {[dot {:millis millis}] ?}] ?}] ?} w* am] 
  #{calc-am}

  [{:hour hour12} {[colon {:minute minute} {[colon {:second secon} {[dot {:millis millis}] ?}] ?}] ?} w* pm]
  #{calc-pm}

  [{:hour hour24} {[colon {:minute minute} {[colon {:second secon} {[dot {:millis millis}] ?}] ?}] ?}]
  #{(fn [b c] (LocalTime. (b :hour 0) (b :minute 0) (b :second 0) (b :millis 0)))}

  [{:hour number} w+ {! "a"} {:minute number} w+ pm] #{calc-pm}
  [{:hour number} w+ {! "a"} {:minute number} w+ am] #{calc-am}
  [{:hour number} w+ {! "a"} {:minute number}]       #{(fn [b c] (LocalTime. (b :hour 0) (b :minute 0) (b :second 0) (b :millis 0)))})


(defrule bound
  end
  whitespace)

(defrule past
  ["half"        w+ "past"] #{30}
  ["quarter"     w+ "past"] #{15}
  [{:min number} w+ "past"] #{:min})

(defrule tohr
  ["quarter" w+ "to"] #{15}
  [{:min number} w+ "to"] #{:min})

(defrule hour24 [{:hour   integer} (? [b c] (and (-> b :hour (< 24)) (-> b :hour (>= 0))))])
(defrule hour12 [{:hour   integer} (? [b c] (and (-> b :hour (<= 12)) (-> b :hour (> 0))))])
(defrule minute [{:minute integer} (? [b c] (-> b :minute (< 60)))])
(defrule secon  [{:second integer} (? [b c] (-> b :second (< 60)))])
(defrule millis
  [digit digit digit] #{(fn [b c] (Integer/parseInt (seqtostring (:ret b))))}
  [digit digit]       #{(fn [b c] (* 10 (Integer/parseInt (seqtostring (:ret b)))))}
  [digit]             #{(fn [b c] (* 100 (Integer/parseInt (seqtostring [(:ret b)]))))})

(defrule am (mklower (mkstr "am")))
(defrule pm (mklower (mkstr "pm")))
(defrule oclock (mklower (mkstr "o'clock")))

(defrule colon \:)
(defrule dot   \.)
