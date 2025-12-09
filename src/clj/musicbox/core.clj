(ns musicbox.core)

(def rads-in-circle (* 2 Math/PI))

(defn calc-diameter [c] (/ c Math/PI))

(defn calc-radius [c] (-> (calc-diameter c) (/ 2)))

(defn z-start [height num-teeth] (-> height (- num-teeth) (/ 2.0) (* -1)))

(defn calc-radians-per-mm [notes]
  (let [num-notes (count notes)]
    (when (> num-notes 0) (/ rads-in-circle (count notes)))))

(defn calc-radians [notes]
  (when-let [rads-per-mm (calc-radians-per-mm notes)]
    (let [num-notes (count notes)]
      (->> (map #(* % rads-per-mm) (range num-notes)) (map float) vec))))

(defn calc-coordinates-of-a-single-pin [{:keys [radius height z-start]} radian note]
  [(* radius (Math/cos radian)) (* radius (Math/sin radian)) (- note z-start)])

(defn calc-pinpoint-coordinates [{:keys [circumference height num-teeth notes] :as params}]
  (let [radius (calc-radius circumference)
        radians (calc-radians notes)
        z-start (z-start height num-teeth)]
    (map (partial calc-coordinates-of-a-single-pin  (assoc params :radius radius :z-start z-start)) radians notes)))
