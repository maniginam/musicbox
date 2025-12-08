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
