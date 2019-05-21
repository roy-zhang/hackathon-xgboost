(ns hackathon.core
  (:require [hackathon.csv :as file]
            [hackathon.format :as format]
            [clj-boost.core :refer :all]))


(def csv1 "feb1_may3.csv")
(def csv2 "nike_locations.csv")
(def csv3 "anomaly-locs.csv")
(def csv4 "clean_dataset.csv")
(def csv5 "bad-ip-events.csv")

(defn fix-ets [events]
  (map #(assoc % :ets (int (:ets %))) events))

(defn remove-same-columns [events]
  (let [same-keys (filter (fn [key] (> 2 (count (set (map key events)))))
                          (keys (first events)))]
    (map #(apply (partial dissoc %) same-keys)
         events)))

(def eventsOld (file/csv-data->maps (file/read-csv csv1)))
(def events (->> csv4
                 file/read-csv
                 file/csv-data->maps
                 fix-ets
                 remove-same-columns))

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

(def new-events (remove-same-columns (fix-ets new-events)))


;; total number of events per kid
;; average distance between subsequent scans
;; average time between subseqent scans
;; average gps coordinate

(defn add-kid-info [events]
  (try
    (let [event-count (count events)
          avg-gps-cood (->> (mapv #(list (:lat %) (:lng %)) events)
                            (apply (partial map +))
                            (reduce +))

          partitioned (partition 2 1 (sort-by :ets events))
          avg-man-dist (->> (map (fn [[e1 e2]] (format/manhattan-dist [(:lat e1) (:lng e1)] [(:lat e2) (:lng e2)]))
                                 partitioned)
                            ((fn [dists] (/ (apply + dists) event-count))))
          avg-sec-apart (->> (map (fn [[e1 e2]] (- (:ets e2) (:ets e1)))
                                  partitioned)
                             ((fn [times] (/ (apply + times) event-count))))]

      (assoc (last events)
       :count event-count
       :avg-gps avg-gps-cood
       :avg-subsq-dist-moved avg-man-dist
       :avg-subsq-sec-dur avg-sec-apart))
    (catch Exception e nil)))

(defn update-values [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(def grouped-events (->> (map add-kid-info (vals (group-by :kid events)))
                        (remove nil?)))


