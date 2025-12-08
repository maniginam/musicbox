(ns musicbox.main
  (:require [musicbox.core :as core]))

(defn -main [& args]
  (println "MAIN")
  (core/build-music-barrel (first args)))
