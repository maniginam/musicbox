(ns musicbox.old-core
  (:require [c3kit.apron.schema :as s]
            [clojure.set :as set]
            [scad-clj.model :as model]
            [scad-clj.scad :as scad]))

(def circumference 45)
(def num-of-teeth 16)                                       ; Number of tines/notes (C4=0 to high C6=14)
(def num-steps 45)                                          ; Time steps (discretize melody)
(def tooth-width 1)
(def cyl-radius (/ circumference Math/PI 2))                ; Cylinder radius (mm)
(def cyl-length (+ (* tooth-width num-of-teeth) 1))         ; Length scales with steps (1mm/step)
(def pin-radius (/ tooth-width 2))                          ; Pin base radius
(def pin-height 3)                                          ; How far pins protrude
(def pi Math/PI)

;; Melody: Vector of vectors. Each inner vec = notes played at that step (0=rest).
;; "Twinkle Twinkle Little Star" discretized (quarter notes, repeat for loop).
(def melody
  ;; Twinkle twinkle (C C G G | A A G2)
  [[4 4 7 7 9 9 7] [0 0] [2 2 4 4 2] [0 0] [5 5 7 7 5] [0 0] [4 4 2 2 4] [0 0]
   ;; Little star (4 4 3 3 | 2 2 1 1 | 5 5 4 4 | 3 3 2]
   [4 4 3 3 2 2 0 0] [5 5 4 4 3 3 2]
   ;; How I wonder... (5 5 4 4 3 3 2 | 5 5 4 4 3 3 2 | 1 1 5 5 | 4 4 3 3 | 2]
   [5 5 4 4 3 3 2] [5 5 4 4 3 3 2] [0 0 5 5] [4 4 3 3] [2]
   ;; Up above... (1 1 5 5 | 4 4 3 3 | 2 2 0 0 | 2]
   [0 0 5 5] [4 4 3 3] [2 2 0 0] [2]])

(def d 14.32394487827058)
(def r 7.1627243913529)
(def inner-height 17.0)
(def height-with-gears 25)

;; Flatten and repeat melody to fill num-steps (loop the tune)
(def full-melody
  ;(cycle (mapcat identity melody))
  (cycle [0 0 7 7 9 9 7
          5 5 4 4 2 2 0
          7 7 5 5 4 4 2
          7 7 5 5 4 4 2
          0 0 7 7 9 9 7
          5 5 4 4 2 2 0]))
; Flatten to single notes per step
(def padded-melody (take num-steps full-melody))

;; Compute pin positions: For each step, if note n > -1, place pin.
(defn pin-positions [melody]
  (for [step (range 1 num-steps)
        :let [note (nth melody step)]
        :when (not= note -1)]
    (let [theta (* (nth melody step) (/ (* 2 pi) num-of-teeth)) ; Angle in radians
          x     (prn "theta: " theta)
          x     (* cyl-radius (Math/cos theta))
          y     (* cyl-radius (Math/sin theta))
          z     (* step (/ cyl-length num-steps))]
      [x y z])))

(defn build-cylinder [{:keys [radius height] :as params}]
  (model/cylinder radius height))

;; Build the model
(defn calc-hertz [n]
  (float (* 440 (Math/pow 2 (double (/ n 12))))))

(defn calc-note [n]
  (float (+ (* 12 (/ (Math/log (/ (calc-hertz n) 440)) (Math/log 2))) 49)))

(defn find-pin-center [note]
  (let [start      (+ (/ (- cyl-length (* num-of-teeth tooth-width)) 2.0) note)
        pin-radius (/ tooth-width 2.0)]
    (+ start pin-radius)))

(defn y-space [notes] (/ circumference (float (count notes))))

(defn calc-song-pins [y-space melody]
  #_(let [theta (* (nth full-melody step) (/ (* 2 pi) num-of-teeth)) ; Angle in radians
          x     (prn "theta: " theta)
          x     (* cyl-radius (Math/cos theta))
          y     (* cyl-radius (Math/sin theta))
          z     (* step (/ cyl-length num-steps))]
      [x y z])
  (map-indexed (fn [i n]
                 (let [theta (* (nth full-melody y-space) (/ (* 2 pi) num-of-teeth))]
                   [(* (Math/cos theta) (calc-note n)) (* (Math/sin theta) (* y-space i)) (* i (/ cyl-length num-of-teeth))])) melody))

;(defn build-model []
;  (let [pins     (map (fn [[x y z]]
;                        (model/translate [x y z]
;                                         (model/cylinder pin-height pin-radius))) ; Pin as small cylinder
;                      (pin-positions) #_(map (partial calc-song-pins (y-space full-melody)) full-melody))
;        x        (prn "pins: " pins)
;        main-cyl (model/cylinder cyl-length (* cyl-radius 1.1)) ; Slightly thicker base
;        ends     (model/union                               ; Elegant rounded ends
;                  (model/translate [0 0 (/ cyl-length -2)]
;                                   (model/cylinder cyl-length cyl-radius))
;                  (model/translate [0 0 (/ cyl-length 2)]
;                                   (model/cylinder 2 cyl-radius)))]
;    (model/union main-cyl ends (model/union pins))))

(defn pin-positions [melody]
  (for [step (range (apply + (map :beats melody)))
        :let [note (:note (nth (cycle melody) (+ 0.0 step)))]
        :when (not= note -1)]
    (let [theta (* note (/ (* 2 Math/PI) num-of-teeth))     ; Angle in radians
          x     (* cyl-radius (Math/cos theta))
          y     (* cyl-radius (Math/sin theta))
          z     (* (+ 0.0 step) (/ cyl-length num-steps))]
      [x y z]))
  )

(defn calc-pin [r {:keys [note i y]}]
  (let [theta (* i (/ (* 2 pi) (+ 5 num-of-teeth)))]
    [(* r (Math/cos theta))
     (* r (Math/sin theta))
     note]))

(defn calc-pin* [r note i]
  (let [theta (* i (/ (* 2 pi) (+ 5 num-of-teeth)))]
    [(* r (Math/cos theta))
     (* r (Math/sin theta))
     note]))

(defn calc-pinpoint [r h steps note step]
  (prn "note: " note)
  (when-not (= -1 note)
    (let [
          theta (* step (/ (* 2 Math/PI) steps))]
      (prn "theta: " theta)
      [(* r (Math/cos theta)) (* r (Math/sin theta)) (- (* step (/ 16 steps)) (/ h 2))])))

(defn calc-pinpoints
  ([{:keys [r notes h]}] (calc-pinpoints r notes h))
  ([r notes h]
   (let [steps (count notes)]
     (prn "steps: " steps)
     (prn "(map #(get notes %) (range steps)): " (map #(get notes %) (range steps)))
     (map #(calc-pinpoint r h steps (get notes %) %) (range steps))
     #_(doseq [step (range notes)]))))

(def coors [[7.16197243913529 0 -6.5]	[7.11354574363979 0.831454251343799 -6.5]	[6.96892054513466 1.65166451028454 0.5]	[6.73005265132749 2.44953884012754 0.5]	[6.40017234070798 3.21428735930222 2.5]	[5.98374067857291 3.93556815600051 2.5]	[5.48638918877086 4.60362714479265 0.5]	[]	[]	[3.58098621956765 6.20245007349516 -1.5]	[2.8367123680716 6.57623845064663 -1.5]	[2.05407684813173 6.86109448418419 -2.5]	[1.24366346255662 7.05316598492019 -2.5]	[0.416431662135065 7.14985551530273 -4.5]	[-0.416431662135064 7.14985551530273 -4.5]	[-1.24366346255662 7.05316598492019 -6.5]	[]	[]	[-3.58098621956764 6.20245007349516 0.5]	[-4.2768333755682 5.74478419930283 0.5]	[-4.91484369700293 5.20942997389965 -1.5]	[-5.48638918877086 4.60362714479265 -1.5]	[-5.98374067857291 3.93556815600051 -2.5]	[-6.40017234070798 3.21428735930222 -2.5]	[-6.73005265132748 2.44953884012754 -4.5]	[]	[]	[-7.16197243913529 8.77088662316859E-16 0.5]	[-7.11354574363979 -0.831454251343798 0.5]	[-6.96892054513466 -1.65166451028454 -1.5]	[-6.73005265132749 -2.44953884012754 -1.5]	[-6.40017234070798 -3.21428735930222 -2.5]	[-5.98374067857291 -3.93556815600051 -2.5]	[-5.48638918877086 -4.60362714479265 -4.5]	[]	[]	[-3.58098621956765 -6.20245007349516 -6.5]	[-2.8367123680716 -6.57623845064662 -6.5]	[-2.05407684813174 -6.86109448418419 0.5]	[-1.24366346255664 -7.05316598492019 0.5]	[-0.41643166213508 -7.14985551530273 2.5]	[0.416431662135046 -7.14985551530274 2.5]	[1.2436634625566 -7.05316598492019 0.5]	[]	[]	[3.58098621956762 -6.20245007349518 -1.5]	[4.27683337556817 -5.74478419930285 -1.5]	[4.9148436970029 -5.20942997389967 -2.5]	[5.48638918877084 -4.60362714479268 -2.5]	[5.98374067857289 -3.93556815600055 -4.5]	[6.40017234070796 -3.21428735930226 -4.5]	[6.73005265132747 -2.44953884012759 -6.5]	[]	[]])
(def rads [0	0.116355283	0.232710567	0.34906585	0.465421134	0.581776417	0.698131701	0.814486984	0.930842268	1.047197551	1.163552835	1.279908118	1.396263402	1.512618685	1.628973969	1.745329252	1.861684535	1.978039819	2.094395102	2.210750386	2.327105669	2.443460953	2.559816236	2.67617152	2.792526803	2.908882087	3.02523737	3.141592654	3.257947937	3.374303221	3.490658504	3.607013787	3.723369071	3.839724354	3.956079638	4.072434921	4.188790205	4.305145488	4.421500772	4.537856055	4.654211339	4.770566622	4.886921906	5.003277189	5.119632473	5.235987756	5.352343039	5.468698323	5.585053606	5.70140889	5.817764173	5.934119457	6.05047474	6.166830024])

(defn build-model [{:keys [d h notes] :as params}]
  (let [r        (/ d 2)
        params   (assoc params :r r)
        pinpoints     (calc-pinpoints params)
        _        (prn "pins: " pinpoints)
        cyl (model/cylinder [1 0.1] 3)
        pins     (map #(when-not (empty? %) (model/translate %1 (model/rotate [0 (* (/ Math/PI 2) (second %)) 0] cyl)))
                   [[7.16197243913529 0 -6.5]	[7.11354574363979 0.831454251343799 -6.5]	[6.96892054513466 1.65166451028454 0.5]	[6.73005265132749 2.44953884012754 0.5]	[6.40017234070798 3.21428735930222 2.5]	[5.98374067857291 3.93556815600051 2.5]])
        x        (prn "pins: " pins)
        main-cyl (model/cylinder (* r 1.1) h)               ; Slightly thicker base
        ends     (model/union                               ; Elegant rounded ends
                   (model/extrude-rotate (model/translate [1 8.5 0] (model/circle 1)) )
                   (model/translate [0 0 (/ h -2)]
                     (model/cylinder r 2))
                   (model/extrude-rotate (model/translate [1 -8.5 0] (model/circle 1)) )
                   (model/translate [0 0 (/ h 2)]
                     (model/cylinder r 2)))]
    (prn "pins: " pins)
    (prn "main-cyl: " main-cyl)
    (prn "(first pins): " (first pinpoints))
    (model/union  pins ends)))

(defn calc-radius [circumference] (/ circumference Math/PI 2))

(defn calc-rhythm-pos [circumference notes]
  (let [num-beats     (apply + (map :beats notes))
        beat-distance (/ circumference num-beats)]
    (->> (reductions (fn [i {:keys [beats]}] (+ i (* beats beat-distance))) 0 (butlast notes))
      (map #(assoc %1 :y %2) notes)
      (map-indexed (fn [i n] (assoc n :i i))))))

(defn build-music-barrel [params]
  (println "BUILD BARREL")
  (let [
        r                                       (calc-radius 45)
        pinpoints #_(calc-rhythm-pos 45 params) (calc-pinpoints params)
        model                                   (build-model params)]
    (spit "scads/musicbox.scad" (scad/write-scad model))))

