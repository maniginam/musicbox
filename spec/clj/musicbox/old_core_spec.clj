(ns musicbox.old-core-spec
		(:require
			[c3kit.apron.schema :as s]
			[speclj.core :refer :all]
			[musicbox.old-core :as sut]))

(def c-scale
		[{:note 0 :beats 1}
			{:note 2 :beats 1}
			{:note 4 :beats 1}
			{:note 5 :beats 1}
			{:note 7 :beats 1}
			{:note 9 :beats 1}
			{:note 11 :beats 1}
			{:note 12 :beats 1}])

(def twinkle [{:note 0 :beats 1} {:note 0 :beats 1} {:note 7 :beats 1} {:note 7 :beats 1} {:note 9 :beats 1} {:note 9 :beats 1} {:note 7 :beats 2}
														{:note 5 :beats 1} {:note 5 :beats 1} {:note 4 :beats 1} {:note 4 :beats 1} {:note 2 :beats 1} {:note 2 :beats 1} {:note 0 :beats 2}
														{:note 7 :beats 1} {:note 7 :beats 1} {:note 5 :beats 1} {:note 5 :beats 1} {:note 4 :beats 1} {:note 4 :beats 1} {:note 2 :beats 2}
														{:note 7 :beats 1} {:note 7 :beats 1} {:note 5 :beats 1} {:note 5 :beats 1} {:note 4 :beats 1} {:note 4 :beats 1} {:note 2 :beats 2}
														{:note 0 :beats 1} {:note 0 :beats 1} {:note 7 :beats 1} {:note 7 :beats 1} {:note 9 :beats 1} {:note 9 :beats 1} {:note 7 :beats 2}
														{:note 5 :beats 1} {:note 5 :beats 1} {:note 4 :beats 1} {:note 4 :beats 1} {:note 2 :beats 1} {:note 2 :beats 1} {:note 0 :beats 2}])

(def twink [0 0 7 7 9 9 7 -1 -1 5 5 4 4 2 2 0 -1 -1 7 7 5 5 4 4 2 -1 -1 7 7 5 5 4 4 2 -1 -1 0 0 7 7 9 9 7 -1 -1 5 5 4 4 2 2 0 -1])

#_(describe "body"

		(it "calcs hertz"
				(should= "466.16376" (str (float (sut/calc-hertz 1))))
				(should= "493.8833" (str (float (sut/calc-hertz 2))))
				(should= "554.36523" (str (float (sut/calc-hertz 4)))))

		(it "calcs the note"
				(should= 49.0 (sut/calc-note 0))
				(should= 50.0 (sut/calc-note 1))
				(should= 51.0 (sut/calc-note 2)))

		(it "places the center of a C4 note pin"
				(should= 1.0 (sut/find-pin-center 0))
				(should= 2.0 (sut/find-pin-center 1))
				(should= 3.0 (sut/find-pin-center 2))
				(should= 4.0 (sut/find-pin-center 3))
				(should= 5.0 (sut/find-pin-center 4))
				(should= 6.0 (sut/find-pin-center 5))
				(should= 7.0 (sut/find-pin-center 6))
				(should= 8.0 (sut/find-pin-center 7))
				)

		(it "draws the cylinder"
				(should= (list :cylinder {:height 4 :radius 2 :center true}) (sut/build-cylinder {:radius 2 :height 4})))

		(it "calcs the rhythm - c-scale quarter notes"
				(should= [0 1.125 2.25 3.375 4.5 5.625 6.75 7.875] (map :y (sut/calc-rhythm-pos 9.0 c-scale)))
				(should= [0 5.625 11.25 16.875 22.5 28.125 33.75 39.375] (map :y (sut/calc-rhythm-pos 45.0 c-scale))))

		(it "calcs the rhythm - twinkle"
				(let [twinkle (sut/calc-rhythm-pos 45.0 twinkle)]
				(should= [0 0.9375 1.875 2.8125 3.75 4.6875 5.625] (map :y (take 7 twinkle)))
				(should= [7.5 8.4375 9.375 10.3125 11.25 12.1875 13.125] (map :y (take 7 (drop 7 twinkle))))
				(should= 0.9375 (- (:y (second twinkle)) (:y (first twinkle))))
				(should= 1.875 (- (:y (nth twinkle 7)) (:y (nth twinkle 6))))))

		(it "blah"
				(should= 1 (first (sut/pin-positions c-scale))))

		(it "calc xyz position of each pin - c-scale"
      (should= [] (map #(sut/calc-pin* 7  (get twink %) %) (range (count twink))))
				(let [notes (sut/calc-rhythm-pos 9.0 c-scale)]
						;(should= [1 2 3] (sut/calc-pin (sut/calc-radius 9) (count c-scale) (first notes)))
						(should= [1 2 3] (sut/calc-pin (sut/calc-radius 9) (first notes)))
						))

    (focus-it "calcs pinpoints"
      (should= [] (sut/build-music-barrel {:d (/ 45 Math/PI) :height 16 :notes twink})))

		#_(it "calcs the pins"
				(should= [{:x 49.0 :y 0.0} {:x 50.0 :y 1.75} {:x 51.0 :y 3.5} {:x 52.0 :y 5.25} {:x 53.0 :y 7.0} {:x 54.0 :y 8.75} {:x 55.0 :y 10.5} {:x 56.0 :y 12.25}] (sut/calc-song-pins (sut/y-space c-scale) c-scale)))

)