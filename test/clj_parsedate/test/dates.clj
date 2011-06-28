(ns clj-parsedate.test.dates
  (:use parsedate.parsedatetime)
  (:use [clojure.test])
  (:import [org.joda.time DateTime Duration Seconds
            ]))

(defn within-one-sec [d1 d2]
  (let [dur (Duration. d1 d2)]
    (-> dur
        (.getStandardSeconds)
        (Math/abs)
        (< 1))))

(deftest test-table
  (let [table '[
                [(parse-date nil) (DateTime.)]
                [(parse-date "")  (DateTime.)]
                [(parse-date "  ") (DateTime.)]
                [(parse-date "now") (DateTime.)]
                [(parse-date "yesterday") (.minus (DateTime.) (Duration/standardDays 1))]
                [(parse-date "11am") (-> (DateTime.)
                                         (.withHourOfDay 11)
                                         (.withMinuteOfHour 0)
                                         (.withSecondOfMinute 0)
                                         (.withMillisOfSecond 0))]
                [(parse-date "4pm") (-> (DateTime.)
                                        (.withHourOfDay 16)
                                        (.withMinuteOfHour 0)
                                        (.withSecondOfMinute 0)
                                        (.withMillisOfSecond 0))]
                [(parse-date "4 pm") (-> (DateTime.)
                                         (.withHourOfDay 16)
                                         (.withMinuteOfHour 0)
                                         (.withSecondOfMinute 0)
                                         (.withMillisOfSecond 0))]
                [(parse-date "4 am") (-> (DateTime.)
                                         (.withHourOfDay 4)
                                         (.withMinuteOfHour 0)
                                         (.withSecondOfMinute 0)
                                         (.withMillisOfSecond 0))]
                [(parse-date "9:45am") (-> (DateTime.)
                                           (.withHourOfDay 9)
                                           (.withMinuteOfHour 45)
                                           (.withSecondOfMinute 0)
                                           (.withMillisOfSecond 0))]
                [(parse-date "9:45pm") (-> (DateTime.)
                                           (.withHourOfDay 21)
                                           (.withMinuteOfHour 45)
                                           (.withSecondOfMinute 0)
                                           (.withMillisOfSecond 0))]
                [(parse-date "yesterday 11:45pm")
                 (-> (DateTime.)
                     (.minusDays 1)
                     (.withHourOfDay 23)
                     (.withMinuteOfHour 45)
                     (.withSecondOfMinute 0)
                     (.withMillisOfSecond 0))]
                [(parse-date "may 27" (-> (DateTime.)
                                          (.withMonthOfYear 7)))
                 (-> (DateTime.)
                     (.withMonthOfYear 5)
                     (.withDayOfMonth 27)
                     (.withHourOfDay 0)
                     (.withMinuteOfHour 0)
                     (.withSecondOfMinute 0)
                     (.withMillisOfSecond 0))]
               ]]
    (doseq [[t e] table]
      (is (within-one-sec (eval t) (eval e)) (str t)))))

(deftest test-tokens
  (let [table [[{:now (DateTime. "2011-06-18T11:09:42.479-03:00")
                 :base (DateTime. "2011-06-18T11:09:42.479-03:00")}
                "now"
                {:base (DateTime. "2011-06-18T11:09:42.479-03:00")}]
               [{:now (DateTime. "2011-06-18T11:09:42.479-03:00")
                 :base (DateTime. "2011-06-18T11:09:42.479-03:00")}
                "yesterday"
                {:base (DateTime. "2011-06-17T11:09:42.479-03:00")}]
               [{:now (DateTime. "2011-06-18T11:09:42.479-03:00")
                 :base (DateTime. "2011-06-18T11:09:42.479-03:00")}
                "tomorrow"
                {:base (DateTime. "2011-06-19T11:09:42.479-03:00")}]
               [{:now (DateTime. "2011-06-18T11:09:42.479-03:00")
                 :base (DateTime. "2011-06-18T11:09:42.479-03:00")}
                "6am"
                {:base (DateTime. "2011-06-18T06:00:00.000-03:00")}]
               [{:now (DateTime. "2011-06-18T11:09:42.479-03:00")
                 :base (DateTime. "2011-06-18T11:09:42.479-03:00")}
                "8am"
                {:base (DateTime. "2011-06-18T08:00:00.000-03:00")}]
               [{:now (DateTime. "2011-06-18T11:09:42.479-03:00")
                 :base (DateTime. "2011-06-18T11:09:42.479-03:00")}
                "8pm"
                {:base (DateTime. "2011-06-18T20:00:00.000-03:00")}]
               [{:now (DateTime. "2011-06-18T11:09:42.479-03:00")
                 :base (DateTime. "2011-06-18T11:09:42.479-03:00")}
                "8"
                {:base (DateTime. "2011-06-18T11:09:42.479-03:00")
                 :last 8}]
               [{:now (DateTime. "2011-06-18T11:09:42.479-03:00")
                 :base (DateTime. "2011-06-18T11:09:42.479-03:00")
                 :last 10}
                "pm"
                {:base (DateTime. "2011-06-18T22:00:00.000-03:00")}]]
        ]
    (doseq [[ctx t e] table]
      (let [r (apply-token ctx t)]
        (doseq [[k v] e]
          (is (= (k r) v) t))))))
