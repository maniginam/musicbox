(ns musicbox.model-spec
  (:require
    [musicbox.core :as core]
    [scad-clj.model :as model]
    [speclj.core :refer :all]
    [musicbox.model :as sut]))

(def twinkle [0 0 7 7 9 9 7 -1 -1 5 5 4 4 2 2 0 -1 -1 7 7 5 5 4 4 2 -1 -1 7 7 5 5 4 4 2 -1 -1 0 0 7 7 9 9 7 -1 -1 5 5 4 4 2 2 0 -1 -1])

(def height 17)
(def circumference 45)
(def num-teeth 16)
(def tooth-width 1)
(def cyl {:height height :circumference circumference :radius (core/calc-radius circumference) :num-teeth num-teeth :tooth-width tooth-width :notes twinkle})

(describe "Model"

  (it "builds a musicbox"
    (sut/build-model cyl))

  (it "calcs the main cylinder"
    (should= nil (sut/build-main-cylinder {}))
    (should= [:cylinder {:h 17 :r (core/calc-radius 45) :center true}] (sut/build-main-cylinder cyl)))

  (context "heads"

    (it "builds the upper head"
      (let [r           (core/calc-radius 45)
            half-height (/ height 2.0)]
        (should= [(list :extrude-rotate {} (list :translate [1 (* half-height 1.1) 0] (list :circle {:r 1})))
                  (list :translate [0 0 (* -1 half-height)] (list :cylinder {:h (* height 0.1) :r (* r 0.75) :center true}))]
          (sut/build-head-with-divot (assoc cyl :radius r :half-height half-height) 1 -1))))

    (it "builds the lower head"
      (let [r           (core/calc-radius 45)
            half-height (/ height 2.0)]
        (should= [(list :extrude-rotate {} (list :translate [1 (* -1.1 half-height) 0] (list :circle {:r 1})))
                  (list :translate [0 0 half-height] (list :cylinder {:h (* height 0.1) :r (* r 0.75) :center true}))]
          (sut/build-head-with-divot (assoc cyl :radius r :half-height half-height) -1 1))))

    (it "builds the heads"
      (should= nil (sut/build-heads {}))
      (let [r           (core/calc-radius 45)
            half-height (/ height 2.0)]
        (should= (concat [:union]
                   (sut/build-head-with-divot (assoc cyl :radius r :half-height half-height) 1 -1)
                   (sut/build-head-with-divot (assoc cyl :radius r :half-height half-height) -1 1))
          (sut/build-heads (assoc cyl :radius r))))))

  (context "pins"

    (it "rotates the standard pin"
      (should= nil (sut/rotate-pin [] 0))
      (should= (list :rotatec [(sut/rotate-X 0) (sut/rotate-Y 0) sut/pin-rotation-rads] (list (list :cylinder {:h 0.0 :r 1 :center true}))) (sut/rotate-pin (model/cylinder 1 0.0) 0))
      (should= (list :rotatec [(sut/rotate-X 1) (sut/rotate-Y 1) sut/pin-rotation-rads] (list (list :cylinder {:h 0.0 :r 1 :center true}))) (sut/rotate-pin (model/cylinder 1 0.0) 1))
      (should= (list :rotatec [(sut/rotate-X Math/PI) (sut/rotate-Y Math/PI) sut/pin-rotation-rads] (list (list :cylinder {:h 0.0 :r 1 :center true}))) (sut/rotate-pin (model/cylinder 1 0.0) Math/PI))
      (should= (list :rotatec [(sut/rotate-X core/rads-in-circle) (sut/rotate-Y core/rads-in-circle) sut/pin-rotation-rads] (list (list :cylinder {:h 0.0 :r 1 :center true}))) (sut/rotate-pin (model/cylinder 1 0.0) core/rads-in-circle))
      (should= (list :rotatec [(sut/rotate-X sut/pin-rotation-rads) (sut/rotate-Y sut/pin-rotation-rads) sut/pin-rotation-rads] (list (list :cylinder {:h 0.0 :r 1 :center true}))) (sut/rotate-pin (model/cylinder 1 0.0) sut/pin-rotation-rads)))

    (it "calcs them all"
      (should= [] (sut/build-pins (assoc cyl :notes [])))
      (should= [(list :translate [7.16197243913529 0.0 -1.2000000000000002] (list :rotatec [1.5707963267948966 0.0 1.5707963267948966] [(list :cylinder {:h 2 :r1 0.5 :r2 0.1 :center true})]))] (sut/build-pins (assoc cyl :notes [0])))
      (should= [(list :translate [7.16197243913529 0.0 -1.2000000000000002] (list :rotatec [1.5707963267948966 0.0 1.5707963267948966] [(list :cylinder {:h 2 :r1 0.5 :r2 0.1 :center true})]))
                (list :translate [-3.580986581057925 6.202449864788637 -0.20000000000000018] (list :rotatec [-0.7853982426811384 1.3603494774012004 1.5707963267948966] [(list :cylinder {:h 2 :r1 0.5 :r2 0.1 :center true})]))
                (list :translate [-3.58098549658705 -6.202450490908147 13.8] (list :rotatec [-0.7853980048300602 -1.3603496147245753 1.5707963267948966] [(list :cylinder {:h 2 :r1 0.5 :r2 0.1 :center true})]))]
        (sut/build-pins (assoc cyl :notes [0 1 15]))))
    )
  )