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


;; of the events in event with label 1.0, what is their lat/long
(defn known-bad-locations [events]
  (->> events
    (filter #(= 1.0 (:label %)))
    (map #(vec (list (:lat %) (:lng %))))
    (remove #(= [0.0 0.0] %))
    set))

(defn known-bad-ips [events]
  (->> events
       (filter #(= 1.0 (:label %)))
       (map #(vec (list (:ip %))))
       set))

;(def new-bad-loc-events (match-events-locs (filter #(= 0.0 (:label %)) events)
;                                           (known-bad-locations events)))
;
;(def new-bad-ip-events (match-events-locs (filter #(= 0.0 (:label %)) events)
;                                          (known-bad-locations events)))

;; (def new-bad-events (map #(assoc % :label 1.0) new-bad-events)) ;; change label to 1.0

(defn switch-label [bad-locations events]
  (concat (filter #(= 1.0 (:label %)) events)
          (map (fn [event]
                 (if (contains? bad-locations [(:lat event) (:lng event)])
                   (assoc event :label 1.0)
                   event))
            (filter #(= 0.0 (:label %)) events))))

(defn switch-label2 [bad-ips events]
  (concat (filter #(= 1.0 (:label %)) events)
          (map (fn [event]
                 (if (contains? bad-ips (:ip event))
                   (assoc event :label 1.0)
                   event))
               (filter #(= 0.0 (:label %)) events))))

(def new-events (->> events
                    (switch-label (known-bad-locations events))
                    (switch-label2 (known-bad-ips events))))

(def new-events (map #(assoc % :ets (int (:ets %))) new-events))

; (file/write-csv
