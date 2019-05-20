(ns hackathon.core
  (:require [hackathon.csv :as file]
            [hackathon.format :as format]))

(def csv1 "feb1_may3.csv")
(def csv2 "nike_locations.csv")
(def csv3 "anomaly-locs.csv")
(def csv4 "clean_dataset.csv")
(def csv5 "bad-ip-events.csv")

(def eventsOld (file/csv-data->maps (file/read-csv csv1)))
(def events (file/csv-data->maps (file/read-csv csv4)))
(def nike-locations (set (rest (map format/convert-nums (file/read-csv csv2)))))
(def anomalys  (set (rest (map format/convert-nums (file/read-csv csv3)))))

(defn match-events-locs [events anomalys]
  (filter (fn [{:keys [lat lng]}]
            (contains? anomalys [lat lng]))
          events))

;; (def bad (match-events-locs events anomalys)) # count 71048
;; (def good (match-events-locs events nike-locations))) # count 2180
;; (def zeros (match-events-locs events #{[0 0}))) # 22984


(defn known-bad-locations [events]
  (->> events
    (filter #(= 1.0 (:label %)))
    (map #(vec (list (:lat %) (:lng %))))
    (remove #(= [0.0 0.0] %))
    set))

(def new-bad-events (match-events-locs (filter #(= 0.0 (:label %)) events)
                                       (known-bad-locations events)))

(defn switch-label [events kids]
  (map (fn [{:keys [kid] :as event}]
         (if (contains? kids kid)
           (assoc event :label 1.0)
           event))
       events))

(def new-events (switch-label events (set (map :kid new-bad-events))))