(ns musicbox.box
  (:require [scad-clj.model :as model]
            [scad-clj.scad :as scad]))

(defn has-all-dimensions? [{:keys [box-height box-length box-thickness]}] (and box-height box-length box-thickness))

(defn calc-core-rectangle [{:keys [height radius box-height box-width box-length box-thickness] :as params}]
  (when (has-all-dimensions? params) (model/translate [(* radius -10) 0 (/ height -2.0)] (model/cube box-length box-width box-thickness {:center true}))))

(defn build-connectors-on-length [{:keys [height radius box-length box-thickness box-width]} y-divider]
  (when (and height radius box-length box-thickness box-width)
    (let [width   (/ box-length 9)
        x       (* radius -10)
        y         (/ box-width y-divider)
        z       (/ height -2.0)]
    (map #(model/translate [(+ x (* % width)) y z] (model/cube 9 4 box-thickness {:center true})) [-3 -1 1 3]))))

(defn add-box-dimensions [{:keys [height] :as params}]
  (when height
    (let [box-length (* 4.0 height)]
      (merge {:box-length box-length :box-width (* 3.0 height) :box-height (/ box-length 2) :box-thickness 3.0} params))))

(defn build-bottom [{:keys [box-height box-length box-thickness] :as params}]
  (when (has-all-dimensions? params)
    (let [main-rectangle   (calc-core-rectangle params)
          front-connectors (build-connectors-on-length params -2.0)
          back-connectors (build-connectors-on-length params 2.0)
          ]
      #_(model/union
          (model/translate [-50 0 (/ box-height 2)] (model/cube 58 44 3))

          (model/translate [-80 -17 (/ box-height 2)] (model/cube 4 4 3))
          (model/translate [-80 -6 (/ box-height 2)] (model/cube 4 4 3))
          (model/translate [-80 5 (/ box-height 2)] (model/cube 4 4 3))
          (model/translate [-80 16 (/ box-height 2)] (model/cube 4 4 3))

          (model/translate [-20 -17 (/ box-height 2)] (model/cube 4 4 3))
          (model/translate [-20 -6 (/ box-height 2)] (model/cube 4 4 3))
          (model/translate [-20 5 (/ box-height 2)] (model/cube 4 4 3))
          (model/translate [-20 16 (/ box-height 2)] (model/cube 4 4 3))

          (model/translate [-71 -24 (/ box-height 2)] (model/cube 9 4 3))
          (model/translate [-57 -24 (/ box-height 2)] (model/cube 9 4 3))
          (model/translate [-43 -24 (/ box-height 2)] (model/cube 9 4 3))
          (model/translate [-29 -24 (/ box-height 2)] (model/cube 9 4 3))

          (model/translate [-71 24 (/ box-height 2)] (model/cube 9 4 3))
          (model/translate [-57 24 (/ box-height 2)] (model/cube 9 4 3))
          (model/translate [-43 24 (/ box-height 2)] (model/cube 9 4 3))
          (model/translate [-29 24 (/ box-height 2)] (model/cube 9 4 3)))
      (apply model/union main-rectangle front-connectors back-connectors))))

(defn build-box [{:keys [height] :as params}]
  (let [params (add-box-dimensions params)
        bottom (build-bottom params)
        front  (model/union (model/translate [-50 -60 (/ height 2)] (model/cube 58 30 3))
                 (model/translate [-80 -73 (/ height 2)] (model/cube 4 4 3))
                 (model/translate [-80 -65 (/ height 2)] (model/cube 4 4 3))
                 (model/translate [-80 -58 (/ height 2)] (model/cube 4 4 3))
                 (model/translate [-80 -51 (/ height 2)] (model/cube 4 4 3))

                 (model/translate [-20 -73 (/ height 2)] (model/cube 4 4 3))
                 (model/translate [-20 -64 (/ height 2)] (model/cube 4 4 3))
                 (model/translate [-20 -57 (/ height 2)] (model/cube 4 4 3))
                 (model/translate [-20 -50 (/ height 2)] (model/cube 4 4 3))

                 (model/translate [-77 -76 (/ height 2)] (model/cube 4 4 3))
                 (model/translate [-64 -76 (/ height 2)] (model/cube 4 4 3))
                 (model/translate [-51 -76 (/ height 2)] (model/cube 4 4 3))
                 (model/translate [-38 -76 (/ height 2)] (model/cube 4 4 3))
                 (model/translate [-24 -76 (/ height 2)] (model/cube 4 4 3))
                 )]
    (->> (model/union bottom)
      scad/write-scad
      (spit "scads/musicbox-box.scad"))))