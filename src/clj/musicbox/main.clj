(ns musicbox.main
  (:require [musicbox.core :as core]
            [scad-clj.scad :as scad]))

(defn build-music-barrel [params]
  (println "BUILD BARREL")
  (->> params
    model/build
    scad/write-scad
    (spit "scads/musicbox.scad")))

(defn -main [& args]
  (println "MAIN")
  (build-music-barrel (first args)))
