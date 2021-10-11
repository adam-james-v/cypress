(ns cypress.fonts
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.composites :as c :refer [svg]]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            [svg-clj.parametric :as p]
            [svg-clj.utils :as utils]
            [svg-clj.tools :as tools]))

(def bentham-glyphs (tools/load-svg-elems "bentham-regular.svg" #{:glyph}))
(def bentham-a (-> bentham-glyphs
                   (->> (filter #(= "a" (get-in % [1 :unicode]))))
                   first
                   (get-in [1 :d])
                   path/path-str->cmds
                   path/cmds->path-string
                   path/path
                   (tf/scale [1 -1])
                   path/decurve))

(def bentham-a-outer-pts (-> bentham-a
                             path/path->elements
                             first
                             (get-in [1 :points])
                             utils/s->v
                             (->> (partition 2))
                             distinct
                             (->> (map vec))))

(def bentham-a-inner-pts (-> bentham-a
                             path/path->elements
                             second
                             (get-in [1 :points])
                             utils/s->v
                             (->> (partition 2))
                             distinct
                             (->> (map vec))))

(def bentham-a-pts (concat
                    (mapv first (partition 7 bentham-a-inner-pts))
                    (mapv first (partition 4 bentham-a-outer-pts))))

(def concave-poly (first (tools/load-svg-elems "drawing.svg" #{:path})))
(def concave-poly-pts (-> concave-poly
                          path/path->elements
                          first
                          (get-in [1 :points])
                          utils/s->v
                          (->> (partition 2))
                          distinct
                          (->> (map vec))))
