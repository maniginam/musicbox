(ns musicbox.core-spec
  (:require
    [speclj.core :refer :all]
    [musicbox.core :as sut]))

(def twinkle [0 0 7 7 9 9 7 -1 -1 5 5 4 4 2 2 0 -1 -1 7 7 5 5 4 4 2 -1 -1 7 7 5 5 4 4 2 -1 -1 0 0 7 7 9 9 7 -1 -1 5 5 4 4 2 2 0 -1 -1])
(def c-scale [0	2	4	5	7	9	11 12])

(def circumference 45)
(def height 17)
(def teeth 16)
(def tooth-width1)

;Diameter	14.32394488
;Radius	7.161972439
;Note Counts	54
;Z-start	1
;Turn	0.833333333
;radians/mm	0.116355283
;mm/radian	7.161972439

(describe "Musicbox"

  (context "calcs"

    (it "diameter"
      (should= 1.0 (sut/calc-diameter Math/PI))
      (should= 2.0 (sut/calc-diameter (* 2 Math/PI)))
      (should= Math/PI (sut/calc-diameter (* Math/PI Math/PI))))

    (it "radius"
      (should= 0.5 (sut/calc-radius Math/PI))
      (should= 1.0 (sut/calc-radius (* 2 Math/PI)))
      (should= (/ Math/PI 2) (sut/calc-radius (* Math/PI Math/PI))))

    (it "calcs z-start"
      (should= -0.5 (sut/z-start 10 9))
      (should= -1.0 (sut/z-start 10 8))
      (should= -0.5 (sut/z-start 20 19))
      (should= -1.0 (sut/z-start 20 18))
      (should= -0.5 (sut/z-start 17 16)))

    (it "radians/mm"
      (should= nil (sut/calc-radians-per-mm  []))
      (should= (* 2 Math/PI) (sut/calc-radians-per-mm [0]))
      (should= Math/PI (sut/calc-radians-per-mm  [0 0]))
      (should= 2.0943 (sut/calc-radians-per-mm  [0 0 0]) 0.01)
      (should= 0.0174532925 (sut/calc-radians-per-mm (repeat 360 0)) 0.000001)
      (should= 0.785398163 (sut/calc-radians-per-mm c-scale) 0.000001)
      (should= 0.116355283 (sut/calc-radians-per-mm twinkle) 0.000001)))

  (it "radians of notes"
    (should= nil (sut/calc-radians []))
    (should= [0.0] (sut/calc-radians [0]))
    (should= (map float [0.0 Math/PI]) (sut/calc-radians [0 0]))
    (should= (map float [0.0	0.7853982	1.5707964	2.3561945	3.1415927	3.9269907	4.712389	5.497787]) (sut/calc-radians c-scale))
    (should= (map float [0.0	0.116355285	0.2327105669325770	0.3490658503988660	0.4654211338651550	0.5817764173314430	0.6981317007977320	0.8144869842640200	0.9308422677303090	1.0471975511966000	1.1635528346628900	1.2799081181291700	1.3962634015954600	1.5126186850617500	1.6289739685280400	1.7453292519943300	1.8616845354606200	1.9780398189269100	2.0943951023932000	2.2107503858594800	2.3271056693257700	2.4434609527920600	2.5598162362583500	2.6761715197246400	2.7925268031909300	2.9088820866572200	3.0252373701235000	3.1415926535897900	3.2579479370560800	3.3743032205223700	3.4906585039886600	3.6070137874549500	3.7233690709212400	3.8397243543875300	3.9560796378538100	4.0724349213201000	4.1887902047863900	4.3051454882526800	4.4215007717189700	4.5378560551852600	4.6542113386515400	4.7705666221178300	4.8869219055841200	5.0032771890504100	5.1196324725167000	5.2359877559829800	5.3523430394492700	5.4686983229155600	5.5850536063818500	5.7014088898481400	5.8177641733144300	5.9341194567807100	6.0504747402470000	6.1668300237132900]) (sut/calc-radians twinkle)))

  (it "calcs coordinates a pin"
    (should= [1.0 0.0 -1] (sut/calc-coordinates-of-a-single-pin {:z-start 1 :height 0 :radius 1 :num-teeth 16 :notes []} 0 0))
    (should= [7.16197243913529 0.0 -1] (sut/calc-coordinates-of-a-single-pin {:z-start 1 :height 17 :radius (sut/calc-radius 45) :num-teeth 16 :notes c-scale} 0 0)))

  (it "calcs all pin coordinates"
    (should= [] (sut/calc-pinpoint-coordinates {:height 0 :circumference 10 :num-teeth 16 :notes []}))
    (should= [[1.5915494309189535 0.0 -8.0]] (sut/calc-pinpoint-coordinates {:height 0 :circumference 10 :num-teeth 16 :notes [0]}))
    (should= [[7.16197243913529 0.0 0.5] [5.064279167700377 5.064279389067064 2.5] [-3.130597704696325E-7 7.161972439135284 4.5] [-5.0642793085791835 5.064279248188258 5.5] [-7.161972439135264 -6.261195409392644E-7 7.5] [-5.064279630530695 -5.0642789262367245 9.5] [8.540566515730392E-8 -7.16197243913529 11.5] [5.064278543894234 -5.064280012873102 12.5]]
      (sut/calc-pinpoint-coordinates {:height 17 :circumference 45 :num-teeth 16 :notes c-scale}))
    )
  )