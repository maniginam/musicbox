(ns musicbox.core)

(def rads-in-circle (* 2 Math/PI))
(defn calc-diameter [c] (/ c Math/PI))
(defn calc-radius [c] (-> (calc-diameter c) (/ 2)))

(defn ten-percent-of [n] (* 0.1 n))
(defn plus-ten-percent [& n] (apply * 1.1 n))
(defn z-start [{:keys [height num-teeth tooth-width]}] (-> height (- (* num-teeth tooth-width)) (/ 2.0) (- (ten-percent-of height)) (* -1)))

(defn calc-radians-per-mm [notes]
  (let [num-notes (count notes)]
    (when (> num-notes 0) (/ rads-in-circle (count notes)))))

(defn calc-radians [notes]
  (when-let [rads-per-mm (calc-radians-per-mm notes)]
    (let [num-notes (count notes)]
      (->> (map #(* % rads-per-mm) (range num-notes)) (map float) vec))))

(defn calc-coordinates-of-a-single-pin [{:keys [radius]} z-start radian note]
  [(* radius (Math/cos radian)) (* radius (Math/sin radian)) (- note z-start)])

(defn calc-pinpoint-coordinates [{:keys [rads notes] :as params}]
  (let [z-start (z-start params)]
    (map (partial calc-coordinates-of-a-single-pin params z-start) rads notes)))
