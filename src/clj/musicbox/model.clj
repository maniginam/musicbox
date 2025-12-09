(ns musicbox.model
  (:require [musicbox.core :as core]
            [scad-clj.model :as model]
            [scad-clj.scad :as scad]))

(defn calc-pins [params]
  (let [pin-cylinder (model/cylinder [1 0.1] 3)]
    (map #(when-not (empty? %) (model/translate %1 pin-cylinder))

      #_(core/calc-pin-coordinates params)
      [[7.16197243913529 0 -6.5] [7.11354574363979 0.831454251343799 -6.5] [6.96892054513466 1.65166451028454 0.5] [6.73005265132749 2.44953884012754 0.5] [6.40017234070798 3.21428735930222 2.5] [5.98374067857291 3.93556815600051 2.5]])))

(defn build-main-cylinder [{:keys [height circumference] :as params}]
  (when-not (empty? params)
    (let [r (core/calc-radius circumference)]
      (model/cylinder r height))))

(defn build-pins [params]
  (let [body (model/cylinder [1 0.1] 3)]
    (map #(when-not (empty? %) (model/translate %1 (model/rotate [0 (* (/ Math/PI 2) (second %)) 0] body)))
      [[7.16197243913529 0 -6.5] [7.11354574363979 0.831454251343799 -6.5] [6.96892054513466 1.65166451028454 0.5] [6.73005265132749 2.44953884012754 0.5] [6.40017234070798 3.21428735930222 2.5] [5.98374067857291 3.93556815600051 2.5]])))

(defn ten-percent-of [n] (* 0.1 n))
(defn plus-ten-percent [& n] (apply * 1.1 n))

(defn build-head-with-divot [{:keys [height half-height radius]} divot-pos z-pos]
  (let [divot-y     (plus-ten-percent divot-pos half-height)
        head-height (ten-percent-of height)]
    [(model/extrude-rotate (model/translate [1 divot-y 0] (model/circle 1)))
     (model/translate [0 0 (* z-pos half-height)] (model/cylinder (* 0.75 radius) head-height))]))

(defn build-heads [{:keys [height] :as params}]
  (when-not (empty? params)
    (let [params     (assoc params :half-height (/ height 2.0))
          upper-head (build-head-with-divot params 1 -1)
          lower-head (build-head-with-divot params -1 1)]
      (apply model/union (concat upper-head lower-head)))))

(defn build-model [{:keys [circumference] :as params}]
  (let [params (assoc params :radius (core/calc-radius circumference))
        barrel (build-main-cylinder params)
        pins   (build-pins params)
        heads  (build-heads params)]
    (->> (model/union barrel pins heads)
      scad/write-scad
      (spit "scads/musicbox.scad"))))