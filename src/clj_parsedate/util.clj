(ns clj-parsedate.util)

(defn readint [s]
  (if (number? s)
    s
    (try
      (Integer/parseInt s)
      (catch Exception e nil))))
