(ns hackathon.csv
  (:require [clojure.data.csv :as csv]
            [hackathon.format :as format]
           [clojure.java.io :as io]))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
       (rest (map format/convert-nums csv-data))))

(defn read-csv [filename]
  (with-open [reader (io/reader filename)]
    (doall
      (csv/read-csv reader))))

(defn write-csv [vecvecs filename]
  (with-open [writer (io/writer filename)]
    (csv/write-csv writer vecvecs)))


(defn write-obj-csv [obj filename]
  (let [header (map name (keys (first obj)))
        vecvecs (concat [header] (map vals obj))]
    (write-csv vecvecs filename)))