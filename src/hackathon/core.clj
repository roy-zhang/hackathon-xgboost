(ns hackathon.core
  (:require [hackathon.csv :as file]
            [hackathon.format :as format]))

(def csv1 "feb1_may3.csv")
(def csv2 "nike_locations.csv")
(def csv3 "anomaly-locs.csv")

(def events (file/csv-data->maps (file/read-csv csv1)))
(def nike-locations (set (rest (map format/convert-nums (file/read-csv csv2)))))
(def anomalys  (set (rest (map format/convert-nums (file/read-csv csv3)))))

(defn match-events-locs [events anomalys]
  (filter (fn [{:keys [lat lng]}]
            (contains? anomalys [lat lng]))
          events))

;; (def bad (match-events-locs events anomalys)) # count 71048
;; (def good (match-events-locs events nike-locations))) # count 2180