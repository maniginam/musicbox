(ns musicbox.model-spec
  (:require
    [musicbox.core :as core]
    [speclj.core :refer :all]
    [musicbox.model :as sut]))

(def twinkle [0 0 7 7 9 9 7 -1 -1 5 5 4 4 2 2 0 -1 -1 7 7 5 5 4 4 2 -1 -1 7 7 5 5 4 4 2 -1 -1 0 0 7 7 9 9 7 -1 -1 5 5 4 4 2 2 0 -1 -1])

(def height 17)
(def circumference 45)
(def num-teeth 16)
(def tooth-width 1)
(def cyl {:height height :circumference circumference :num-teeth num-teeth :tooth-width tooth-width :notes twinkle})

(describe "Model"

  (it "builds a musicbox"
    (sut/build-model cyl))

  (it "calcs the main cylinder"
    (should= nil (sut/build-main-cylinder {}))
    (should= [:cylinder {:h 17 :r (core/calc-radius 45) :center true}] (sut/build-main-cylinder cyl)))

  (context "heads"

    (it "builds the upper head"
      (let [r (core/calc-radius 45)
            half-height (/ height 2.0)]
        (should= [(list :extrude-rotate {} (list :translate [1 (* half-height 1.1) 0] (list :circle {:r 1})))
                  (list :translate [0 0 (* -1 half-height)] (list :cylinder {:h (* height 0.1) :r (* r 0.75) :center true}))]
          (sut/build-head-with-divot (assoc cyl :radius r :half-height half-height) 1 -1))))

    (it "builds the lower head"
      (let [r (core/calc-radius 45)
            half-height (/ height 2.0)]
        (should= [(list :extrude-rotate {} (list :translate [1 (* -1.1 half-height) 0] (list :circle {:r 1})))
                  (list :translate [0 0 half-height] (list :cylinder {:h (* height 0.1) :r (* r 0.75) :center true}))]
          (sut/build-head-with-divot (assoc cyl :radius r :half-height half-height) -1 1))))

  (it "builds the heads"
    (should= nil (sut/build-heads {}))
    (let [r (core/calc-radius 45)
          half-height (/ height 2.0)]
      (should= (concat [:union]
                 (sut/build-head-with-divot (assoc cyl :radius r :half-height half-height) 1 -1)
                 (sut/build-head-with-divot (assoc cyl :radius r :half-height half-height) -1 1))
        (sut/build-heads (assoc cyl :radius r))))))
  )