(ns cypress.main
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.composites :as c :refer [svg]]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            [svg-clj.parametric :as p]
            [svg-clj.utils :as utils]
            [svg-clj.tools :as tools]
            [hiccup.core :refer [html]]))

(defn shift
  [pts n]
  (vec (take (count pts) (drop n (cycle pts)))))

(def a-style {:fill "none"
              :stroke "#d8dee9"
              :opacity 0.8
              :stroke-width 1.5
              :stroke-linejoin "round"
              :stroke-linecap "round"})

(def b-style {:fill "#e5e9f0"
              :stroke "#81a1c1"
              :opacity 0.125
              :stroke-width 1.5
              :stroke-linejoin "round"
              :stroke-linecap "round"})

(def c-style {:fill "none"
              :stroke "#81a1c1"
              :stroke-width 2.5
              :stroke-linejoin "round"
              :stroke-linecap "round"})

(defn- pline
  [line]
  (let [[_ {:keys [x1 y1 x2 y2]}] line]
    (p/line [x1 y1] [x2 y2])))

(defn sketch-line
  [a b]
  (let [dr 0.75
        dg 35
        l (el/line a b)
        lu (tf/offset l dr)
        ld (tf/offset l (- dr))
        rays (->> (range 0 dg)
                  (map #(/ % dg))
                  (map (juxt (pline lu) (pline ld)))
                  (map #(apply el/line %))
                  shuffle
                  (drop (* dg 0.5))
                  (sort-by #(utils/distance a ((pline %) 0.5))))
        pts (map #((pline %) (/ (rand-int 100) 100)) rays)]
    (-> (concat [a] pts [b])
        (el/polyline))))

(def sketch-line-ex
  (-> (sketch-line [0 0] [200 100])
      (tf/style a-style)))

(defn sketch-polygon
  [pts]
  (let [pts (concat pts [(first pts)])
        lines (map #(apply sketch-line %) (partition 2 1 pts))]
    (-> lines
        (path/elements->path)
        (path/path->elements)
        first
        (assoc 0 :polygon))))

(defn random-pts
  [w h n]
  (let [[wh hh] (map #(/ % 2) [w h])
        f #(vector (rand-int w) (rand-int h))]
    (->> (repeatedly n f)
         (map #(utils/v- % [wh hh])))))

(defn hull
  ([pts]
   (hull [{:pt (first (sort-by first pts))}] pts))
  ([acc pts]
   (if (or (= (count acc) (count pts))
           (and (< 1 (count acc))
                (= (:pt (first acc)) (:pt (last acc)))))
     (drop-last acc)
     (let [prev (:pt (last acc))
           dir (if (= 1 (count acc))
                 (utils/v+ [0 1] prev)
                 (:pt (last (drop-last acc))))
           f (fn [pt]
               (let [a (when (= 3 (count (into #{} [dir prev pt])))
                         (utils/angle-from-pts dir prev pt))]
                 {:pt pt :angle a :abc [dir prev pt]}))
           sorted (->> (map f pts)
                       (remove #(nil? (:angle %)))
                       (sort-by #(Math/abs (- (:angle %) 180))))]
       (recur (conj acc (first sorted)) pts)))))

(defn hull-check []
  (let [pts (random-pts 240 350 50)
        hull (map :pt (hull pts))
        ptf #(-> (el/circle 2)
                 (tf/translate %)
                 (tf/style {:fill "red"}))]
    (el/g
     (-> hull
         sketch-polygon
         (tf/style a-style))
     (apply el/g (map ptf pts))
     (-> (el/line (first hull) (utils/v+ [0 51] (first hull)))
         (tf/style {:stroke "pink"}))
     (-> (el/circle 3) (tf/translate (first hull)) (tf/style {:fill "green"}))
     (-> (el/circle 3) (tf/translate (second hull)) (tf/style {:fill "blue"})))))

(defn converging-lines
  [tri n]
  (let [dr 0.0125
        pt (get (vec tri) (rand-int 3))
        l (apply p/line (remove #{pt} (into #{} tri)))]
    (map #(list (l (+ (float (/ % n)) (- (rand (* 2 dr)) dr)))
                pt) (rest (range n)))))

(defn linerider-lines
  [tri n]
  (let [dr 0.0125
        [la lb] (->> (shuffle tri)
                     (partition 2 1)
                     (map #(apply p/line %)))]
    (map #(list (la (+ (float (/ % n)) (- (rand (* 2 dr)) dr)))
                (lb (+ (float (/ % n)) (- (rand (* 2 dr)) dr))))
         (rest (range n)))))

;; no randomness
(defn linerider-lines-nr
  [tri n]
  (let [[la lb] (->> tri
                     (partition 2 1)
                     (map #(apply p/line %)))]
    (map #(list (la (/ % n))
                (lb (/ % n)))
         (range (inc n)))))

(defn fence-lines
  [pts n]
  (let [dr 0.0125
        [la lb lc] (->> (shift pts (rand-int (count pts)))
                        (partition 2 1)
                        (map #(apply p/line %)))
        fb (if lc
             (fn [t] (lc (- 1 t)))
             (fn [t] (lb (- 1 t))))]
    (map #(list (la (+ (float (/ % n)) (- (rand (* 2 dr)) dr)))
                (fb (+ (float (/ % n)) (- (rand (* 2 dr)) dr))))
         (rest (range n)))))

(defn hatch-lines
  [pts n]
  (let [dr 0.0125
        [la lb lc ld] (->> pts
                           (#(concat % [(first %)]))
                           (partition 2 1)
                           (map #(apply p/line %)))
        lb (fn [t] (lb (- 1 t)))
        lc (if ld (fn [t] (lc (- 1 t))) lc)
        [fa fb fc fd] (if ld [la lc lb ld] [la lb lb lc])]
    (concat
     (map #(list (fa (+ (float (/ % n)) (- (rand (* 2 dr)) dr)))
                 (fb (+ (float (/ % n)) (- (rand (* 2 dr)) dr))))
          (rest (range n)))
     (map #(list (fc (+ (float (/ % n)) (- (rand (* 2 dr)) dr)))
                 (fd (+ (float (/ % n)) (- (rand (* 2 dr)) dr))))
          (rest (range n))))))

(defn- new-center
  [tri]
  (let [ctr (mapv float (utils/centroid-of-pts tri))
        [la lb lc] (map #(p/line ctr %) tri)
        l (apply p/line (take 2 (shuffle (map #(% (rand 0.6)) [la lb lc]))))]
    (l (rand))))

(defn inset-lines
  [pts n]
  (let [dr 0.025
        ctr (new-center pts)
        rays (map #(p/line % ctr) pts)
        rayfn (fn [t]
                (vec
                 (map #(% (+ (float (/ t n)) (- (rand (* 2 dr)) dr))) rays)))]
    (map rayfn (rest (range n)))))

(defn no-fill [pts _] [pts])

(defn convex?
  [pts]
  (= (set (:pts (hull pts))) (set pts)))

(def tri-fill-strategies [no-fill
                          no-fill
                          converging-lines
                          converging-lines
                          fence-lines
                          fence-lines
                          hatch-lines
                          hatch-lines
                          linerider-lines
                          linerider-lines
                          inset-lines
                          inset-lines])

(def quad-fill-strategies [no-fill
                           fence-lines
                           hatch-lines
                           inset-lines])

(defn tri-fill-check []
  (let [tri [[0 0] [200 -20] [30 150]]
        f (get tri-fill-strategies (rand-int (count tri-fill-strategies)))
        lines (f tri (+ 9 (rand-int 7)))
        f (fn [pts]
            (let [sk (if (< 2 (count pts))
                       sketch-polygon
                       #(apply sketch-line %))]
              (-> pts sk (tf/style a-style))))]
    (el/g
     (apply el/g (map f lines))
     (-> tri
         sketch-polygon
         (tf/style a-style)
         (tf/style {:stroke "blue"})))))

(defn quad-fill-check []
  (let [quad [[0 0] [200 -20] [190 190] [90 30]]
        f (get quad-fill-strategies (rand-int (count quad-fill-strategies)))
        lines (f quad (+ 9 (rand-int 7)))
        f (fn [pts]
            (let [sk (if (< 2 (count pts))
                       sketch-polygon
                       #(apply sketch-line %))]
              (-> pts sk (tf/style a-style))))]
    (el/g
     (apply el/g (map f lines))
     (-> quad
         sketch-polygon
         (tf/style a-style)
         (tf/style {:stroke "blue"})))))

(def abs #?(:clj #(Math/abs %)  :cljs js/Math.abs))
(def pow #?(:clj #(Math/pow %1 %2) :cljs js/Math.pow))

;; https://gist.github.com/mutoo/5617691
(defn circumscribe-tri
  [[[ax ay] [bx by] [cx cy]]]
  (let [A (- bx ax)
        B (- by ay)
        C (- cx ax)
        D (- cy ay)
        E (+ (* A (+ ax bx)) (* B (+ ay by)))
        F (+ (* C (+ ax cx)) (* D (+ ay cy)))
        G (* 2 (- (* A (- cy by)) (* B (- cx bx))))]
    (when (> (abs G) 0.000001)
      (let [cx (/ (- (* D E) (* B F)) G)
            cy (/ (- (* A F) (* C E)) G)
            dx (- cx ax)
            dy (- cy ay)
            r  (+ (pow dx 2) (pow dy 2))]
        {:x cx :y cy :radius-squared r}))))

(defn edges [[p1 p2 p3]] [[p1 p2] [p2 p3] [p3 p1]])

(defn contains-pt?
  [{:keys [x y radius-squared]} [px py]]
  (let [distance-squared (+ (pow (- x px) 2) (pow (- y py) 2))]
    (< distance-squared radius-squared)))

(defn outer-edges
  [tris]
  (let [all-edges (mapcat edges tris)
        matches (fn [edge] (filter #{edge (reverse edge)} all-edges))
        appears-once (fn [edge] (= (count (matches edge)) 1))]
    (filter appears-once all-edges)))

(defn make-new-tris
  [containers pt]
  (->> containers
       outer-edges
       (map (fn [[p1 p2]] [p1 p2 pt]))
       set))

(defn add-pt-to-tris
  [tris pt]
  (let [containers (filter #(contains-pt? (circumscribe-tri %) pt) tris)
        new-tris (make-new-tris containers pt)]
    (set/union (set/difference tris containers) new-tris)))

;; http://paulbourke.net/papers/triangulate/
(defn triangulate
  [pts]
  (let [pts (map (fn [[x y]] [(float x) (float y)]) pts)
        [bl br tr tl] (map #(utils/v* % [2 2]) (utils/bounds-of-pts pts))
        initial #{[tl tr bl] [bl tr br]}
        with-bounds (reduce add-pt-to-tris initial pts)
        tris (remove #(some #{tl tr bl br} %) with-bounds)]
    {:pts pts
     :tris tris
     :edges (distinct (mapcat edges tris))}))

(def tria [[0 0] [100 0] [0 100]])
(def trib [[0 0] [100 0] [50 -100]])

(defn- share-edge?
  [tria trib]
  (let [edges (into #{} (map set (concat (edges tria) (edges trib))))]
    (= (count edges) 5)))

(defn join-tris
  [tria trib]
  (when (share-edge? tria trib)
    (let [pt-groups (group-by identity (concat tria trib))
          [a c b d] (map first (sort-by count (vals pt-groups)))]
      (list [a b c d]))))

(defn get-random-neighbour
  [tri tris]
  (->> tris
       (remove #{tri})
       (filter #(share-edge? tri %))
       shuffle
       first))

(defn randomly-glue-tris
  ([tris n]
   (randomly-glue-tris [] tris n))
  ([quads tris n]
   (if (or (empty? tris) (= n (count quads)))
       {:quads quads
        :tris tris}
     (let [tria (first (shuffle tris))
           trib (get-random-neighbour tria tris)
           quad (join-tris tria trib)]
       (recur (concat quads quad) (vec (remove #{tria trib} tris)) n)))))

(def bg "#455D7A")
(def lcol "#F95959")
(def fg "#BA6375")

(def bg-style {:fill bg})
(def fg-style {:fill fg})

(def line-style {:fill "none"
                 :stroke lcol
                 :stroke-width 1.75
                 :stroke-linejoin "round"
                 :stroke-linecap "round"})

(def thin-line-style {:fill "none"
                      :stroke lcol
                      :stroke-width 0.75
                      :stroke-linejoin "round"
                      :stroke-linecap "round"})

(defn fill-tri
  [tri]
  (let [trif (first (shuffle tri-fill-strategies))
        lines (trif tri (+ 9 (rand-int 7)))
        f (fn [pts]
            (let [sk (if (< 2 (count pts))
                       el/polygon
                       #(apply el/line %))]
              (-> pts
                  sk
                  (tf/style (first (shuffle [line-style thin-line-style]))))))]
    (el/g
     (apply el/g (map f lines)))))

(defn fill-quad
  [quad]
  (let [quadf (first (shuffle quad-fill-strategies))
        lines (quadf quad (+ 7 (rand-int 4)))
        f (fn [pts]
            (let [sk (if (< 2 (count pts))
                       el/polygon
                       #(apply el/line %))]
              (-> pts
                  sk
                  (tf/style (first (shuffle [line-style thin-line-style]))))))]
    (el/g
     (apply el/g (map f lines)))))

(defn gen-art
  [w h n bg?]
  (let [pts (random-pts w h n)
        tris (:tris (triangulate pts))
        hull (map :pt (hull pts))
        trif #(-> % sketch-polygon (tf/style thin-line-style))
        trifb #(-> % sketch-polygon (tf/style (merge fg-style {:opacity 0.5})))
        [_ trihl] (split-at (* 0.25 (count tris)) (vec (shuffle tris)))]
    (el/g
     (when bg? (-> (el/rect (* 1.25 w) (* 1.25 h)) (tf/style bg-style)))
     ;; fill some triangles
     (apply el/g (map trifb trihl))
     ;; inner lines of triangles
     (apply el/g (map fill-tri tris))
     ;; outline all triangles
     (apply el/g (map trif tris))
     ;; outline the hull
     (-> hull sketch-polygon (tf/style line-style)))))

(defn fill-tri2
  [tri n]
  (let [trif linerider-lines-nr
        lines (trif tri n)
        f (fn [pts]
            (let [sk (if (< 2 (count pts))
                       el/polygon
                       #(apply el/line %))]
              (-> pts sk (tf/style thin-line-style))))]
    (el/g
     (apply el/g (map f lines)))))

(defn- tri-polar-angle
  [tri focus]
  (let [ctr (utils/centroid-of-pts tri)]
    (utils/angle-from-pts (utils/v- ctr focus)
                          [0 0]
                          (utils/v- [1 0] focus))))

(defn gen-art2
  [r n]
  (let [focus [0 0]
        segs 9
        pts #_(random-pts 500 700 20) (conj (p/regular-polygon-pts r n) focus)
        tris (->> pts
                  triangulate
                  :tris
                  (map #(sort-by (fn [pt] (utils/distance pt focus)) %))
                  (sort-by #(tri-polar-angle % focus))
                  (map #(shift % 2)))
        hull (map :pt (hull pts))
        trif #(-> % sketch-polygon (tf/style a-style))]
    (el/g
     (apply el/g (map #(fill-tri2 % segs) tris))
     #_(-> hull sketch-polygon (tf/style c-style)))))

(defn gen-art3
  [w h n bg?]
  (let [pts (random-pts w h n)
        seed-tris (:tris (triangulate pts))
        mesh (randomly-glue-tris seed-tris (int (/ n 4)))
        quads (:quads mesh)
        tris (:tris mesh)
        hull (map :pt (hull pts))
        trif #(-> % sketch-polygon (tf/style line-style))
        quadf #(-> % sketch-polygon (tf/style line-style))
        trifb #(-> % sketch-polygon (tf/style (merge fg-style {:opacity 0.5})))
        [_ trihl] (split-at (* 0.25 (count tris)) (vec (shuffle tris)))]
    (el/g
     (when bg? (-> (el/rect (* 1.25 w) (* 1.25 h)) (tf/style bg-style)))
     ;; fill some triangles
     (apply el/g (map trifb trihl))
 
     ;; quads
     (apply el/g (map fill-quad quads))
     (apply el/g (map quadf quads))

     ;; tris
     (apply el/g (map fill-tri tris))
     (apply el/g (map trif tris))

     ;; hull
     (-> hull sketch-polygon (tf/style line-style)))))

(spit "gen-art3.svg" (html (gen-art3 500 700 24 true)))

(defn gen-grid3
  []
  (let [results
        #_(map #(gen-art2 80 %) (range 3 20))
        (repeatedly 9 #(gen-art3 220 220 16 false))
        grid (map #(vec (reverse %)) (p/rect-grid 3 3 230 230))]
    (lo/distribute-on-pts results grid)))
