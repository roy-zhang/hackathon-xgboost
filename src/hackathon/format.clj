(ns hackathon.format)

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

; calcs abs(x1-x2) + abs(y1-y2) < manhattan-margin
(defn is-close? [manhattan-margin a b]
  (< (apply + (map #(Math/abs (- %1 %2)) a b))
     manhattan-margin))