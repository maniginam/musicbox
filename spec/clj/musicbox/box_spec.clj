(ns musicbox.box-spec
  (:require
    [musicbox.core :as core]
    [speclj.core :refer :all]
    [musicbox.box :as sut]))

(def params {:height 17 :radius (core/calc-radius 45)})
(def params-1 {:height 1 :radius 1})

(describe "Box"

  (it "builds the box" (sut/build-box params))

  (it "adds the core dimensions of the box"
    (should= nil (sut/add-box-dimensions {}))
    (should= {:height 1 :box-length 4.0 :box-height 2.0 :box-width 3.0 :box-thickness 3.0} (sut/add-box-dimensions {:height 1}))
    (should= (assoc params :box-length 68.0 :box-height 34.0 :box-width 51.0 :box-thickness 3.0) (sut/add-box-dimensions params)))

  (it "does not overwrite existing dimensions"
    (should= {:height 2 :box-length 58.0 :box-height 44 :box-width 6.0 :box-thickness 5} (sut/add-box-dimensions {:height 2 :box-length 58.0 :box-height 44 :box-thickness 5})))

  (context "components"

    (it "calcs the core rectangle to a component"
      (should= nil (-> {} sut/add-box-dimensions sut/calc-core-rectangle))
      (should= (list :translate [-10 0 -0.5] (list :cube {:x 4.0 :y 3.0 :z 3.0 :center true})) (-> params-1 sut/add-box-dimensions sut/calc-core-rectangle))
      (should= (list :translate [(* (:radius params) -10) 0 (/ 17.0 -2)] (list :cube {:x 68.0 :y 51.0 :z 3.0 :center true})) (-> params sut/add-box-dimensions sut/calc-core-rectangle)))

    (context "calcs the connectors for bottom"
      (it "front"
        (should= nil (sut/build-connectors-on-length {} nil))
        (should= [(list :translate [-11.333333333333334 -1.5 -0.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-10.444444444444445 -1.5 -0.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-9.555555555555555 -1.5 -0.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-8.666666666666666 -1.5 -0.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))]
          (sut/build-connectors-on-length (sut/add-box-dimensions params-1) -2))
        (should= [(list :translate [-94.28639105801957 -25.5 -8.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-79.17527994690846 -25.5 -8.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-64.06416883579735 -25.5 -8.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-48.95305772468624 -25.5 -8.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))]
          (sut/build-connectors-on-length (sut/add-box-dimensions params) -2)))

      (it "back"
        (should= nil (sut/build-connectors-on-length {} 2))
        (should= [(list :translate [-11.333333333333334 1.5 -0.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-10.444444444444445 1.5 -0.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-9.555555555555555 1.5 -0.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-8.666666666666666 1.5 -0.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))]
          (sut/build-connectors-on-length (sut/add-box-dimensions params-1) 2))
        (should= [(list :translate [-94.28639105801957 25.5 -8.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-79.17527994690846 25.5 -8.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-64.06416883579735 25.5 -8.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))
                  (list :translate [-48.95305772468624 25.5 -8.5] (list :cube {:x 9 :y 4 :z 3.0 :center true}))]
          (sut/build-connectors-on-length (sut/add-box-dimensions params) 2)))

      )


    (it "builds the bottom"
      (should= nil (sut/build-bottom {}))
      (let [params (sut/add-box-dimensions params-1)]
        (should= (apply list :union (sut/calc-core-rectangle params) (sut/build-connectors-on-length params -2.0) (sut/build-connectors-on-length params 2.0)) (sut/build-bottom params)))
      )

    )
  )