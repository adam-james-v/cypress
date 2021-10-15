(ns cypress.animate
  (:require [clojure.string :as str]
            [svg-clj.tools :as tools]
            [clojure.java.shell :as sh :refer [sh]]))

(defn- anim-frames! [f name framerate dur]
  (let [mkdir (sh "mkdir" "-pv" name)
        frames (int (* framerate dur))
        framefn (fn [fr]
                  (let [fname (format (str name "/%03d") fr)]
                    (do (tools/save-svg (f (/ fr frames)) (str fname ".svg"))
                        (sh "/usr/local/bin/inkscape"
                            (str "--export-filename=" fname ".png")
                            (str fname ".svg"))
                        #_(sh "/usr/local/bin/convert" (str fname ".svg") (str fname ".png")))))]
    (when (= 0 (:exit mkdir))
        (into [] (map framefn (range 1 (inc frames)))))))

(defn- anim-video! [name framerate]
  (let [ffmpeg 
        (sh "/usr/local/bin/ffmpeg" "-f" "image2" "-r" (str framerate)
            "-i" (str name "/%03d.png")
            "-c:v" "libvpx-vp9" "-vf" "format=rgba"
            "-pix_fmt" "yuva420p" "-b:v" "800k"
            "-y" (str name ".webm"))]
    (when (= 0 (:exit ffmpeg))
      #_(sh "rm" "-rf" name))))

(defn animate! [{:keys [graphics-fn name framerate duration]}]
  (do (anim-frames! graphics-fn name framerate duration)
      (anim-video! name framerate)))
