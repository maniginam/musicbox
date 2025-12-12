(ns musicbox.model
  (:require [musicbox.core :as core]
            [musicbox.box :as box]
            [scad-clj.model :as model]
            [scad-clj.scad :as scad]))

(defn calc-pins [params]
  (let [pin-cylinder (model/cylinder [1 0.1] 3)]
    (map #(when-not (empty? %) (model/translate %1 pin-cylinder))

      #_(core/calc-pin-coordinates params)
      [[7.16197243913529 0 -6.5] [7.11354574363979 0.831454251343799 -6.5] [6.96892054513466 1.65166451028454 0.5] [6.73005265132749 2.44953884012754 0.5] [6.40017234070798 3.21428735930222 2.5] [5.98374067857291 3.93556815600051 2.5]])))

(def pin-rotation-rads (/ Math/PI 2))

(defn rotate-X [rads] (* pin-rotation-rads (Math/cos rads)))
(defn rotate-Y [rads] (* pin-rotation-rads (Math/sin rads)))

(defn rotate-pin [pin rads]
  (when (seq pin)
    (model/rotate [(rotate-X rads) (rotate-Y rads) pin-rotation-rads] pin)))

(defn build-pins [{:keys [notes] :as params}]
  (let [pin       (model/cylinder [0.5 0.1] 2)
        rads      (core/calc-radians notes)
        pin-coors (core/calc-pinpoint-coordinates (assoc params :rads rads))]
    (->> rads (map #(rotate-pin pin %)) (map model/translate pin-coors))))

(defn build-head-with-divot [{:keys [height half-height radius]} divot-pos z-pos]
  (let [divot-y     (core/plus-ten-percent divot-pos half-height)
        head-height (core/ten-percent-of height)]
    [(model/extrude-rotate (model/translate [1 divot-y 0] (model/circle 1)))
     (model/translate [0 0 (* z-pos half-height)] (model/cylinder (* 0.75 radius) head-height))]))

(defn build-heads [{:keys [height] :as params}]
  (when-not (empty? params)
    (let [params     (assoc params :half-height (/ height 2.0))
          upper-head (build-head-with-divot params 1 -1)
          lower-head (build-head-with-divot params -1 1)]
      (apply model/union (concat upper-head lower-head)))))

(defn build-main-cylinder [{:keys [height radius] :as params}]
  (when-not (empty? params) (model/cylinder radius height)))

(defn build-barrel [{:keys [circumference height] :as params}]
  (let [params (-> (assoc params :radius (core/calc-radius circumference)) box/add-box-dimensions)
        barrel (build-main-cylinder params)
        pins   (build-pins params)
        heads  (build-heads params)
        bottom (box/build-bottom params)]
    (->> (model/union barrel heads pins bottom)
      scad/write-scad
      (spit "scads/musicbox-box.scad"))))