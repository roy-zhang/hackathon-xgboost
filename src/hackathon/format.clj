(ns hackathon.format
  (:require [clj-time.coerce :as time]))


(defn round
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn convert-nums [i]
  (mapv #(try (->> %
                   clojure.string/trim
                   read-string
                   (round 3))
              (catch Exception _ %)) i))

(defn manhattan-dist [a b]
  (apply + (map #(Math/abs (- %1 %2)) a b)))

; calcs abs(x1-x2) + abs(y1-y2) < manhattan-margin
(defn is-close? [manhattan-margin a b]
  (< (manhattan-dist a b)
     manhattan-margin))

(defn convert-unix-time [events]
  (map (fn [event]
         (update event :ets time/to-long)))
  time/to-long)