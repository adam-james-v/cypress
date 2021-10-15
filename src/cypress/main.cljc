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
            #?(:clj [svg-clj.tools :as tools])
            #?(:clj [cypress.animate :refer [animate!]])))

(defn shift
  [pts n]
  (vec (take (count pts) (drop n (cycle pts)))))

(defn rcol
  []
  (let [r (- 255 (rand-int 200))
        g (- 255 (rand-int 200))
        b (- 255 (rand-int 200))]
    {:r r :g g :b b
     :css (str "rgb(" r "," g "," b ")")}))

(defn- brightness-idx
  [{:keys [r g b]}]
  (/ (+ (* r 299) (* g 587) (* b 114)) 1000))

(defn brightness-diff
  [cola colb]
  (apply - (reverse (sort (map brightness-idx [cola colb])))))

(defn hue-diff
  [cola colb]
  (let [dr (apply - (reverse (sort [(:r cola) (:r colb)])))
        dg (apply - (reverse (sort [(:g cola) (:g colb)])))
        db (apply - (reverse (sort [(:b cola) (:b colb)])))]
    (+ dr dg db)))

(defn contrast-score
  [cola colb]
  (let [brightness-diff (brightness-diff cola colb)
        hue-diff (hue-diff cola colb)]
    (+ (- 500 hue-diff) (- 125 brightness-diff))))

(defn random-cols
  [n]
  (let [cols (repeatedly (* 3 n) rcol)
        cola (first (shuffle cols))]
    (take n (reverse (sort-by #(contrast-score cola %) cols)))))

(defn- pline
  [line]
  (let [[_ {:keys [x1 y1 x2 y2]}] line]
    (p/line [x1 y1] [x2 y2])))

(defn sketch-line
  [a b]
  (let [dr 0.75
        dg 15
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
  ([pts] (hull [{:pt (first (sort-by first pts))}] pts))
  ([acc pts]
   (if (or
        ;; stop the process if acc grows larger than the pts count
        (> (count acc) (count pts))
        ;; *should* always end where the last added point closes the poly
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

(defn nested-hull
  ([pts] (nested-hull [] pts))
  ([acc pts]
   (if (> 3 (count pts))
     acc
     (let [hull (hull pts)
           npts (remove (set (map :pt hull)) pts)]
       (recur (conj acc hull) npts)))))

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

(defn edges [pts]
  (partition 2 1 (conj (vec pts) (first pts))))

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
     #_#_:edges (distinct (mapcat edges tris))}))

(defn fillet-pts
  [pts r]
  (let [fillet (p/regular-polygon-pts r 50)
        ipts (tf/offset-pts pts (- r))
        f (fn [pt] (map #(utils/v+ pt %) fillet))
        npts (mapcat f ipts)]
    (map :pt (hull npts))))

(defn chamfer-pts
  [pts r]
  (let [fillet (p/regular-polygon-pts r 50)
        ipts (tf/offset-pts pts (- r))
        f (fn [pt] (map #(utils/v+ pt %) fillet))
        npts (mapcat f ipts)]
    (->> (hull npts)
         (map :pt)
         (partition 2 1)
         (sort-by #(apply utils/distance %))
         reverse
         (take (count pts))
         (apply concat)
         hull
         (map :pt))))

(def iso-euler-angles [35.264 45 0])
(def origin-angle-adjust-a [90 0 0])
(def origin-angle-adjust-b [0 -90 0])

(defn- sin-cos-pair [theta]
  [(Math/sin ^long (utils/to-rad theta))
   (Math/cos ^long (utils/to-rad theta))])

(defn- rot-pt-2d
  [[x y] theta]
  (let [[s-t c-t] (sin-cos-pair theta)]
    [(- (* x c-t) (* y s-t))
     (+ (* y c-t) (* x s-t))]))

;; this rotates a point around [0,0,0]
(defn- rot-pt
  [[x y z] axis theta]
  (cond
    (= axis :x) (into [x] (rot-pt-2d [y z] theta))
    (= axis :y) (apply #(into [] [%2 y %1]) (rot-pt-2d [z x] theta))
    (= axis :z) (into (rot-pt-2d [x y] theta) [z])))

(defn- rotate-point
  [pt [ax ay az]]
  (let [pt (if (< (count pt) 3)
             (conj pt 0)
             pt)]
    (-> pt
        (rot-pt :z az)
        (rot-pt :y ay)
        (rot-pt :x ax))))

(defn- rotate-points
  [pts [ax ay az]]
  (mapv #(rotate-point % [ax ay az]) pts))

(defn isometric-xf
  [pts]
  (-> pts
      (rotate-points origin-angle-adjust-a)
      (rotate-points origin-angle-adjust-b)
      (rotate-points iso-euler-angles)))

(defn top-xf
  [pts]
  (-> pts
      (rotate-points [0 0 0])))

(defn right-xf
  [pts]
  (-> pts
      (rotate-points [90 0 0])))

(defn- edge-to-right?
  [pt edge]
  (let [[px _] pt
        [w _] (tf/bb-dims (el/polyline (conj edge pt)))
        [[ax ay] [bx by]] edge
        [ix iy] (utils/line-intersection [pt (utils/v+ pt [(* 30 w) 0])] edge)]
    (when ix
      (and (<= (min ax bx) ix (max ax bx))
           (<= (min ay by) iy (max ay by))
           (<= px ix)))))

(defn- edge-strictly-to-right?
  [pt edge]
  (let [[px _] pt
        [w _] (tf/bb-dims (el/polyline (conj edge pt)))
        [[ax ay] [bx by]] edge
        [ix iy] (utils/line-intersection [pt (utils/v+ pt [(* 30 w) 0])] edge)]
    (when ix
      (and (< (min ax bx) ix (max ax bx))
           (< (min ay by) iy (max ay by))
           (< px ix)))))

(defn pt-strictly-inside?
  [pt poly-pts]
  (let [[x y] pt
        edges (edges poly-pts)]
    (odd? (count (filter #(edge-strictly-to-right? pt %) edges)))))

(defn pt-inside?
  [pt poly-pts]
  (let [[x y] pt
        edges (edges poly-pts)]
    (odd? (count (filter #(edge-to-right? pt %) edges)))))

(defn cut-away-tris
  [{:keys [tris] :as data} boundary-pts]
  (let [tri-inside? #(pt-inside? (utils/centroid-of-pts %) boundary-pts)]
    (assoc data :tris (vec (filter tri-inside? tris)))))

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

(defn dots
  [pts n]
  (let [n (* 2 n)
        dr 0.5
        [corner _ _ _] (utils/bounds-of-pts pts)
        spacing (/ (apply max (utils/bb-dims pts)) (float n))
        dotf (fn [pt]
               (->> (p/regular-polygon-pts (+ 0.5 (- (rand (* 2 dr)) dr)) 5)
                    (map #(utils/v- pt %))))
        dot-positions (->> (p/rect-grid (* 2 n) (* 2 n) spacing spacing)
                           (map #(utils/v- % [(* 2 spacing) (* 2 spacing)]))
                           (map #(utils/v+ % corner))
                           (filter #(pt-strictly-inside? % pts)))]
    (map dotf dot-positions)))

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

(def tri-fill-strategies [dots
                          no-fill
                          converging-lines
                          fence-lines
                          hatch-lines
                          linerider-lines
                          inset-lines])

(def quad-fill-strategies [dots
                           no-fill
                           fence-lines
                           hatch-lines
                           inset-lines])

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
  ([{:keys [tris] :as data} n]
   (randomly-glue-tris [] tris n data))
  ([quads tris n data]
   (if (or (empty? tris) (= n (count quads)))
       (merge data
              {:quads (vec quads)
               :tris tris})
     (let [tria (first (shuffle tris))
           trib (get-random-neighbour tria tris)
           quad (join-tris tria trib)]
       (recur (concat quads quad) (vec (remove #{tria trib} tris)) n data)))))

(defn ease-in-sin
  [t]
  (- 1 (Math/cos (/ (* Math/PI t) 2))))

(defn ease-out-sin
  [t]
  (Math/sin (/ (* Math/PI t) 2)))

(defn stroke-pts
  [curve width n-segments]
  (let [tlns (->> (el/line [0 0] [0 (* 0.5 width)])
                  (repeat (inc n-segments))
                  (#(lo/distribute-on-curve % curve))
                  (map pline))
        blns (->> (el/line [0 0] [0 (* -0.5 width)])
                  (repeat n-segments)
                  (#(lo/distribute-on-curve % curve))
                  (map pline))]
    (concat [(curve 0)]
            (map #(%1 0) tlns)
            [(curve 1)]
            (reverse (map #(%1 0) blns)))))

(defn tapered-stroke-pts
  [curve width n-segments taper-t]
  (let [taper-n (int (* n-segments taper-t))
        taper (map #(ease-out-sin (/ % taper-n)) (range taper-n))
        dist (concat taper (repeat (- n-segments (* 2 (count taper))) 1) (reverse taper))
        tlns (->> (el/line [0 0] [0 (* 0.5 width)])
                  (repeat (inc n-segments))
                  (#(lo/distribute-on-curve % curve))
                  (map pline))
        blns (->> (el/line [0 0] [0 (* -0.5 width)])
                  (repeat n-segments)
                  (#(lo/distribute-on-curve % curve))
                  (map pline))]
    (concat [(curve 0)]
            (map #(%1 (* 1 (- 1 %2))) tlns dist)
            [(curve 1)]
            (reverse (map #(%1 (* 1 (- 1 %2))) blns dist)))))

(defn get-inflections
  [pts]
  (let [b-pts (reverse pts)
        a-triples (partition 3 1 (concat [(last pts)] (vec pts) [(first pts)]))
        b-triples (partition 3 1 (concat [(last b-pts)] (vec b-pts) [(first b-pts)]))
        a (map second
               (filter #(< 180 (apply utils/angle-from-pts %)) a-triples))
        b (map second
               (filter #(< 180 (apply utils/angle-from-pts %)) b-triples))]
    (when-not (or (= (count a) (count pts))
                  (= (count b) (count pts)))
      (first (sort-by count [a b])))))

(defn hull-check []
  (let [pts (random-pts 240 350 50)
        hull (map :pt (hull pts))
        ptf #(-> (el/circle 2)
                 (tf/translate %)
                 (tf/style {:fill "red"}))]
    (el/g
     (-> hull
         sketch-polygon
         (tf/style {:fill "none" :stroke "skyblue"}))
     (apply el/g (map ptf pts))
     (-> (el/line (first hull) (utils/v+ [0 51] (first hull)))
         (tf/style {:stroke "pink"}))
     (-> (el/circle 3) (tf/translate (first hull)) (tf/style {:fill "green"}))
     (-> (el/circle 3) (tf/translate (second hull)) (tf/style {:fill "blue"})))))

(defn tri-fill-check []
  (let [tri [[0 0] [200 -20] [30 150]]
        f (get tri-fill-strategies (rand-int (count tri-fill-strategies)))
        lines (f tri (+ 9 (rand-int 7)))
        f (fn [pts]
            (let [sk (if (< 2 (count pts))
                       sketch-polygon
                       #(apply sketch-line %))]
              (-> pts sk (tf/style {:fill "none" :stroke "white"}))))]
    (el/g
     (apply el/g (map f lines))
     (-> tri
         sketch-polygon
         (tf/style {:fill "none" :stroke "skyblue"})
         (tf/style {:stroke "blue"})))))

(defn quad-fill-check []
  (let [quad [[0 0] [200 -20] [190 190] [90 120]]
        f (get quad-fill-strategies (rand-int (count quad-fill-strategies)))
        lines (f quad (+ 9 (rand-int 7)))
        f (fn [pts]
            (let [sk (if (< 2 (count pts))
                       sketch-polygon
                       #(apply sketch-line %))]
              (-> pts sk (tf/style {:fill "none" :stroke "skyblue"}))))]
    (el/g
     (apply el/g (map f lines))
     (-> quad
         sketch-polygon
         (tf/style {:fill "none" :stroke "skyblue"})
         (tf/style {:stroke "blue"})))))

(defn tapered-bezier-pts
  [w h lt]
  (let [[wh hh] (map #(/ % 2.0) [w h])]
    (tapered-stroke-pts
     (p/bezier
      [[0 (- hh)]
       [(* (rand) (* w -1)) (* (rand) (* hh -1))]
       [(* (rand) (* w 1)) (* (rand) (* hh 1))] 
       [0 hh]]) lt 16 0.5)))

(defn regular-poly-sets-pts
  [rmax n-rings n-max]
  (let [ns (reverse (range 1 (inc n-rings)))
        rs (map #(* rmax (/ % n-rings)) ns)
        segs (map #(max (int (* n-max (/ % n-rings))) 3) ns)]
  (mapcat #(p/regular-polygon-pts %1 %2) rs segs)))

;; 2. generate mesh from pts
(defn gen-mesh
  [pts {:keys [quads-n concave]}]
  (let [data (-> pts triangulate (assoc :hull (mapv :pt (hull pts))))]
    (cond-> data
      concave (cut-away-tris pts)
      quads-n (randomly-glue-tris quads-n))))

;; 3. create fills for tris and quads
(defn- fill-tris
  [{:keys [tris] :as data}]
  (let [f (fn [tri]
            (let [fillfn (first (shuffle tri-fill-strategies))]
              (fillfn tri (+ 4 (rand-int 6)))))]
    (assoc data :tri-fills (mapv f tris))))

(defn- fill-quads
  [{:keys [quads] :as data}]
  (let [f (fn [quad]
            (let [fillfn (first (shuffle quad-fill-strategies))]
              (fillfn quad (+ 4 (rand-int 6)))))]
    (assoc data :quad-fills (mapv f quads))))

(defn gen-fills
  [data opts]
  (-> data
      fill-tris
      fill-quads))

(defn gen-cols
  [data {:keys [fg bg hl] :as opts}]
  (let [cols (random-cols 32)]
    (-> data
        (assoc :cols {:fg (if fg {:css fg} (first cols))
                      :bg (if bg {:css bg} (last cols))
                      :hl (if hl {:css hl} (nth cols 4))}))))

(defn- render-fill
  [fill-data {:keys [fg]}]
  (let [f (fn [pts]
            (if (= 2 (count pts))
              (apply el/line pts)
              (el/polygon pts)))]
    (map #(-> (f %)
              (tf/style {:fill "none"
                         :stroke (:css fg)
                         :stroke-width 1.5}))
         fill-data)))

(defn gen-render
  [{:keys [tri-fills tris
           quad-fills quads
           cols
           hull] :as data}
   {:keys [bg]}]
  (let [hull-poly (el/polygon hull)
        ctr (utils/centroid-of-pts (tf/bounds hull-poly))
        pad 50
        [w h] (map #(+ (* 2 pad) %) (tf/bb-dims hull-poly))
        outline {:fill "none" :stroke (:css (:fg cols)) :stroke-width 1.5}]
    (assoc data :render
           (el/g
            ;; bg
            (when bg
              (-> (el/rect w h)
                  (tf/translate ctr)
                  (tf/style {:fill (:css (:bg cols))})))
            ;; tris
            (when-not (empty? tris)
              (concat
                (map #(-> (el/polygon %) (tf/style outline)) tris)
                (map #(render-fill % cols) tri-fills)))
            ;; quads
            (when-not (empty? quads)
              (concat
                (map #(-> (el/polygon %) (tf/style outline)) quads)
                (map #(render-fill % cols) quad-fills)))))))

(defn gen-data
  [pts opts]
  (-> pts
      (gen-mesh opts)
      (gen-fills opts)
      (gen-cols opts)
      (gen-render opts)))

(defn squared-grid
  [nx ny]
  (let [[hnx hny] (map #(int (/ % 2)) [nx ny])
        xs (map #(double (/ % hnx)) (range 0 (inc hnx)))
        xs (concat xs (rest (reverse xs)))
        ys (map #(double (/ % hny)) (range 0 (inc hny)))
        ys (concat ys (rest (reverse ys)))]
    (for [x xs y ys] (* x y))))

(def asdf
  (let [min-r 0.1
        max-r 20
        n 7
        grid (p/rect-grid n n 50 50)
        gf (squared-grid n n)
        ctr (utils/centroid-of-pts grid)
        xf-grid (map #((p/line %1 ctr) (* 0.5 (- 1 %2))) grid gf)]
  (lo/distribute-on-pts
   (map #(-> (el/circle (+ min-r (* max-r %)))) gf)
   xf-grid)))

(defn art-grid
  []
  (let [opts {:quads-n 4}
        n 7
        grid (p/rect-grid n n 320 320)
        gf (squared-grid n n)
        ctr (utils/centroid-of-pts grid)
        xf-grid (map #((p/line %1 ctr) (* 0.4 (- 1 %2))) grid gf)
        tmp (remove #(< (:gf %) 0.00001)
                    (map (fn [gf grid] {:gf gf :grid grid})
                         gf xf-grid))
        gf (map :gf tmp)
        xf-grid (map :grid tmp)
        shapes (map #(->
                      (random-pts (+ 70 (* 210 %))
                                  (+ 70 (* 210 %))
                                  (+ 7 (int (* 18 %))))
                      (gen-data opts))
                    gf)
        cols (:cols (first shapes))
        shapes (map #(-> %
                         (assoc :cols cols)
                         (gen-render opts))
                    shapes)
        res (lo/distribute-on-pts (map :render shapes) xf-grid)
        pad 200
        [w h] (map #(+ (* 2 pad) %) (tf/bb-dims res))]
    (el/g
     (-> (el/rect w h)
         (tf/translate ctr)
         (tf/style {:fill (:css (:bg cols))}))
     res)))

(defn sinwave
  [amp freq]
  (fn [t]
    (* amp (Math/sin (* t freq Math/PI)))))

(defn blend
  [fa fb alpha]
  (fn [t]
    (let [line (p/line (fa t) (fb t))]
      (line alpha))))

(defn- remap-within
  [f [start end] x]
  (when (and (>= x start) (< x end))
    (let [step (- end start)
          t (/ (- x start) step)]
      (f t))))

(defn eased-polyline
  [pts easefn]
  (let [step (/ 1.0 (dec (count pts)))
        lines (map (partial apply p/line) (partition 2 1 pts))
        length (reduce + (map #(:length (%)) lines))
        intervals (->> lines
                       (map #(:length (%)))
                       (reductions +)
                       (concat [0])
                       (map #(/ % length))
                       (partition 2 1))
        easedlines (map #(fn [t] (% (easefn t))) lines)]
    (fn
      ([] {:fn `polyline
           :input [pts]
           :length length})
      ([t]
       (cond
         (= (float t) 0.0) (first pts)
         (= (float t) 1.0) (last pts)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) easedlines intervals))))))))

(defn multiblend
  ([fs alpha]
   (fn [t]
     (let [line (p/polyline (map #(% t) fs))]
       (line alpha))))
  ([fs easefn alpha]
   (fn [t]
     (let [line (eased-polyline (map #(% t) fs) easefn)]
       (line alpha))))) 

(defn fn-offset
  [curve f]
  (let [eps 0.000001]
    (fn [t]
      (let [t (cond (<= (- 1 eps) t) (- 1 eps)
                    (> eps t) eps
                    :else t)
            n (utils/normalize (utils/normal (curve (- t eps)) (curve (+ t eps))))
            tpt (curve t)
            l (p/line tpt (utils/v+ tpt n))]
        (l (f t))))))

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
  (let [focus [0 0] #_[(rand-int (* r 0.875)) (* r 0.875)]
        segs 9
        pts #_(random-pts 400 500 20) (conj (p/regular-polygon-pts r n) focus)
        tris (->> pts
                  triangulate
                  :tris
                  (map #(sort-by (fn [pt] (utils/distance pt focus)) %))
                  (sort-by #(tri-polar-angle % focus))
                  (map #(shift % 2)))
        hull (map :pt (hull pts))
        trif #(-> % sketch-polygon (tf/style {:fill "none" :stroke "skyblue"}))]
    (el/g
     (apply el/g (map #(fill-tri2 % segs) tris))
     #_(-> hull el/polygon
         (tf/style thin-line-style)))))

(defn gen-art3
  [w h n bg?]
  (let [pts (random-pts w h n)
        seed-tris (:tris (triangulate pts))
        mesh (randomly-glue-tris seed-tris (int (/ n 4)))
        quads (:quads mesh)
        tris (:tris mesh)
        hull (map :pt (hull pts))
        trif #(-> % sketch-polygon
                  (tf/style line-style))
        quadf #(-> % sketch-polygon
                   (tf/style line-style))
        trifb #(-> % sketch-polygon
                   (tf/style (merge fg-style {:opacity 0.5})))
        [_ trihl] (split-at (* 0.25 (count tris)) (vec (shuffle tris)))]
    (el/g
     (when bg? (-> (el/rect (* 1.25 w) (* 1.25 h))
                   (tf/style bg-style)))
     ;; fill some triangles
     (apply el/g (map trifb trihl))
 
     ;; quads
     (apply el/g (map fill-quad quads))
     (apply el/g (map quadf quads))

     ;; tris
     (apply el/g (map fill-tri tris))
     (apply el/g (map trif tris))

     ;; hull-line
     (-> hull sketch-polygon
         (tf/style line-style)))))

#_(tools/save-svg "gen-art3.svg" (svg-clj.composites/svg (gen-art3 500 700 24 true)))

(defn blob
  [w h blend-factor]
  (let [max-r (* 0.27 (min w h))
        min-r (* 0.07 (min w h))
        pts (->>
             (for [pt (random-pts (- w max-r) (- h max-r) 5)]
               (let [r (+ (rand-int min-r) (- max-r min-r))
                     pts (p/regular-polygon-pts r 45)]
                 (map #(utils/v+ % pt) pts)))
             (apply concat)
             hull
             (map :pt))
        fa (p/polygon pts)
        r (/ (apply max (utils/bb-dims pts)) 2.0)
        fb (-> (p/circle (* 0.75 r))
               (p/translate (utils/centroid-of-pts pts))
               (p/rotate 180))
        f (blend fb fa blend-factor)
        tmp-pts (map f (range 0 1.005 0.005))
        [fw fh] (utils/bb-dims tmp-pts)
        ctr (utils/centroid-of-pts tmp-pts)
        sc (apply min (map / [w h] [fw fh]))]
    (-> f
        (p/translate (utils/v* [-1 -1] ctr))
        (p/scale [sc sc]))))

(defn good-blob
  []
  (fn-offset (blob 400 400 3) (sinwave 4 20)))

(defn hair
  []
  (-> (el/line [0 0] [0 (+ (+ 7 (rand-int 7)))])
      (tf/style {:stroke "black"
                 :stroke-linecap "round"
                 :stroke-linejoin "round"
                 :stroke-width 2})))

(defn spike
  [w h]
  (let [shape (el/polyline [[(* -0.5 w) 0] [0 (- h)] [(* 0.5 w) 0]])
        ctr (tf/centroid shape)]
  (-> shape
      (tf/translate (utils/v* [-2 -2] ctr))
      (tf/style {:stroke "black"
                 :stroke-linecap "round"
                 :stroke-linejoin "round"
                 :fill "white"
                 :stroke-width 2}))))

(defn semi-circle
  [r]
  (let [shape (-> (p/regular-polygon-pts r 24)
                  (->> (take 13))
                  el/polyline)
        ctr (tf/centroid shape)]
  (-> shape
      (tf/rotate 180)
      (tf/style {:stroke "black"
                 :fill "white"
                 :stroke-linejoin "round"
                 :stroke-width 2}))))

(defn bubble
  []
  (-> (p/regular-polygon-pts (+ 5 (rand-int 12)) 11)
      sketch-polygon
      (tf/translate [0 (- (+ 17 (rand-int 40)))])
      (tf/style {:stroke "black"
                 :fill "rgba(255,255,255,0.12)"
                 :opacity (rand)
                 :stroke-width (inc (rand 1))})))

(defn amoeba
  []
  (let [c-start (good-blob)
        c-mid (good-blob)
        c-end (good-blob)
        n-spikes 85
        n-paths 15
        n-bubbles 15
        hairs (repeatedly (* 4 n-spikes) hair)
        bubbles (shuffle (concat
                         (repeatedly n-bubbles bubble)))
        c (fn [t] (multiblend [c-start
                               c-mid
                               c-end
                               c-start] t))]
    (fn [t]
      (let [c (c t)
            pts (map c (range 0 1 0.0125))
            inner-paths (inset-lines pts n-paths)
            pf #(-> (el/polygon %2)
                    (tf/style {:fill "none"
                               :stroke-width 1
                               :stroke "black"
                               :opacity (* 0.625 (- 1 (/ %1 n-paths)))}))
            spike-l (->> (range 0 n-spikes)
                         (take 2)
                         (map #(c (float (/ % n-spikes))))
                         (apply utils/distance))]
        (el/g
         (-> (el/polygon pts)
             (tf/style {:fill "black"
                        :opacity 0.35}))
         (map-indexed pf inner-paths)
         (->> (lo/distribute-on-curve hairs c)
              drop-last)
         (->> (lo/distribute-on-curve (repeat n-spikes (semi-circle (/ spike-l 2.0))) c)
              drop-last
              (partition 2)
              (map first))
         (->> (lo/distribute-on-curve (repeat n-spikes (spike spike-l 10)) c)
              drop-last
              (partition 2)
              (map second))
         (->> (lo/distribute-on-curve bubbles c))
         (-> (el/polygon pts)
             (tf/style {:fill "none"
                        :stroke-width 3
                        :stroke "black"})))))))

(def a (amoeba))

(def amoeba-anim
  (let [amoeba a]
    {:name "amoeba"
     :framerate 24
     :duration 2
     :graphics-fn
     (fn [t]
       (-> (el/g
            (-> (el/rect 500 500)
                (tf/translate [250 250])
                (tf/style {:fill "white"}))
            (-> t
                (amoeba)
                (tf/translate [250 250])))
           (svg 500 500)))}))

(defn ease-in-out-sin
  [t]
  (/ (- (Math/cos (* Math/PI t)) 1) -2))

(defn blend-pillar
  []
  (let [r 70
        s (-> (p/circle 2) (p/rotate 180))
        a (-> (blob 100 100 7) (p/translate (first (random-pts (- r) r 1))))
        b (-> (blob 120 120 0.3) (p/translate (first (random-pts (- r) r 1))))
        c (-> (blob 80 80 3) (p/translate (first (random-pts (- r) r 1))))
        d (-> (blob 100 100 3) (p/translate (first (random-pts (- r) r 1))))
        e (-> (blob 120 120 0.3) (p/translate (first (random-pts (- r) r 1))))
        f (-> (blob 60 60 5) (p/translate (first (random-pts (- r) r 1))))
        shapefn #(multiblend [s a b c d e f s] ease-in-out-sin %)
        z 2
        n 270
        f (fn [layer]
            (-> (shapefn (/ layer n))
                (map (range 0 1 0.01))
                isometric-xf
                el/polygon
                (tf/translate [0 (* layer z 1)])
                (tf/style {:fill "rgba(255,255,255,0.4)"
                           :stroke "black"
                           :stroke-width 1})))]
    (-> (el/g (reverse (map f (range 0 (inc n)))))
        #_(tf/rotate 90))))

(defn blend-pillar-anim-fn
  []
  (let [r 70
        s (-> (p/circle 2) (p/rotate 180))
        a1 #(-> (blob 100 100 7) (p/translate (first (random-pts (- r) r 1))))
        b1 #(-> (blob 120 120 0.3) (p/translate (first (random-pts (- r) r 1))))
        c1 #(-> (blob 80 80 3) (p/translate (first (random-pts (- r) r 1))))
        d1 #(-> (blob 100 100 3) (p/translate (first (random-pts (- r) r 1))))
        e1 #(-> (blob 120 120 0.3) (p/translate (first (random-pts (- r) r 1))))
        f1 #(-> (blob 60 60 5) (p/translate (first (random-pts (- r) r 1))))
        aa (repeatedly 5 a1)
        bb (repeatedly 3 b1)
        cc (repeatedly 7 c1)
        dd (repeatedly 9 d1)
        ee (repeatedly 4 e1)
        ff (repeatedly 3 f1)
        af #(multiblend (concat aa [(first aa)]) ease-in-out-sin %)
        bf #(multiblend (concat bb [(first bb)]) ease-in-out-sin %)
        cf #(multiblend (concat cc [(first cc)]) ease-in-out-sin %)
        df #(multiblend (concat dd [(first dd)]) ease-in-out-sin %)
        ef #(multiblend (concat ee [(first ee)]) ease-in-out-sin %)
        ff #(multiblend (concat ff [(first ff)]) ease-in-out-sin %)
        z 3
        n 170
        animfn (fn [t]
                 #(multiblend
                  [s (af t) (bf t) (cf t) (df t) (ef t) (ff t) s]
                  ease-in-out-sin %))]
    (fn [t]
      (let [bg (-> (el/rect 700 700) (tf/style {:fill "white"}))
            shapefn (animfn t)
            f (fn [layer]
                (-> (shapefn (/ layer n))
                    (map (range 0 1 0.01))
                    isometric-xf
                    el/polygon
                    (tf/translate [0 (* layer z 1)])
                    (tf/style {:fill "white"
                               :stroke "black"
                               :stroke-width 1})))
            shape (el/g (reverse (map f (range 0 (inc n)))))
            ctr (tf/centroid shape)]
        (el/g
         bg
         (-> shape
             (tf/translate (utils/v* [-1 -1] ctr))))))))

(defn three-pillars
  []
  (let [[a b c] (repeatedly 3 blend-pillar)
        shape (el/g (-> a (tf/translate [0 0]))
                    (-> b (tf/translate [250 0]))
                    (-> c (tf/translate [500 0])))
        bg (-> (el/polygon (tf/bounds shape))
               (tf/offset 125)
               (tf/style {:fill "white"}))]
    (el/g bg shape)))

(def pillar-anim (blend-pillar-anim-fn))

(def blend-pillar-anim
  {:name "blend-pillar"
   :framerate 48
   :duration 15
   :graphics-fn pillar-anim})

#_(animate! blend-pillar-anim)

(def cell-size 12)
(def base-style {:fill "none"
                 :stroke "#83aa9d"
                 :stroke-width 2
                 :stroke-linecap "round"})

(defn line-tile
  [w h] 
   (-> (el/line [w 0] [0 h])
       (tf/style base-style)))

(defn tri-tile
  [w h]
  (-> (el/polygon [[0 h] [w h] [w 0]])
      (tf/style {:fill "white"})))

(defn quarter-circle-tile
  [w h]
  (let [[wh hh] (map #(/ % 2.0) [w h])]
    (el/g
     #_(-> (el/rect w h)
         (tf/translate [wh hh])
         (tf/style {:fill "none"}))
     (-> (p/arc [wh h] [(* 1.275 wh) (* 1.275 hh)] [w hh])
         (map (range 0.00001 1 0.01))
         path/polyline
         (tf/style base-style)
         (tf/style {:fill "none"}))
     (-> (p/arc [0 hh] [(* 0.725 wh) (* 0.725 hh)] [wh 0])
         (map (range 0.00001 1 0.01))
         path/polyline
         (tf/style base-style)
         (tf/style {:fill "none"})))))

(defn funtile
  [w h]
  (let [[wh hh] (map #(/ % 2.0) [w h])
        r (* 0.125 (min w h))]
    (el/g
     (-> (el/rect w h)
         (tf/translate [wh hh])
         (tf/style {:fill "none"}))
     (-> (el/line [wh (+ r hh)] [wh h])
         (tf/style base-style))
     (-> (el/line [(+ wh r) hh] [w hh])
         (tf/style base-style))
     (-> (el/circle r)
         (tf/translate [wh hh])
         (tf/style base-style)))))

(defn funtile2
  [w h]
  (let [p 0.3
        [wh hh] (map #(/ % 2.0) [w h])
        [wq hq] (map #(* % 0.375 p) [w h])
        r (* 0.0875 (min w h))
        gl (p/line [0 0] [w h])
        apt (gl p)
        bpt (gl (- 1 p))]
    (el/g
     (-> (el/rect w h)
         (tf/translate [wh hh])
         (tf/style {:fill "none"}))
     
     (-> (el/circle r)
         (tf/translate apt)
         (tf/style base-style))
     (-> (el/polyline [[0 hh] [wq hh] apt])
         (tf/style base-style))
     (-> (el/polyline [[wh 0] [wh hq] apt])
         (tf/style base-style))
     
     (-> (el/circle r)
         (tf/translate bpt)
         (tf/style base-style))
     (-> (el/polyline [[w hh] [(- w wq) hh] bpt])
         (tf/style base-style))
     (-> (el/polyline [[wh h] [wh (- h hq)] bpt])
         (tf/style base-style)))))

(defn truchet-grid
  [tileset nx ny cellw cellh]
  (let [grid (map reverse (p/rect-grid ny nx cellh cellw))
        tiles (map #(% cellw cellh) tileset)]
    (-> (repeatedly #(first (shuffle tiles)))
        (lo/distribute-on-pts grid))))

(defn rotate-around
  [tile deg pin]
  (let [octr (tf/centroid tile)
        nctr (utils/rotate-pt-around-center octr deg pin)]
    (-> tile
        ;; move shape's centroid to 0 0
        (tf/translate (utils/v* [-1 -1] octr))
        (tf/rotate deg)
        ;; move shape to final position
        (tf/translate nctr))))

(defn bb-rotate
  [tile deg]
  (let [bctr (utils/centroid-of-pts (tf/bounds tile))
        ctr (tf/centroid tile)
        nctr (utils/rotate-pt-around-center ctr deg bctr)]
    (-> tile
        #_(tf/translate (utils/v* [-1 -1] ctr))
        (tf/translate (utils/v- nctr ctr))
        (tf/rotate deg))))

(defn make-rect-tileset
  [tilefn]
  (let [f (fn [deg w h]
            (let [[w h] (if (#{0 180} deg) [w h] [h w])
                  bctr (utils/centroid-of-pts (tf/bounds (tilefn w h)))]
              (-> (tilefn w h)
                  (tf/translate (utils/v* [-1 -1] bctr))
                  (tf/rotate deg))))]
    (map #(partial f %) [0 90 180 270])))

(def a
  (let [cells (-> (concat
                   (make-rect-tileset quarter-circle-tile)
                   (make-rect-tileset funtile)
                   (make-rect-tileset funtile2)
                   #_(make-rect-tileset line-tile))
                  (truchet-grid 24 24 32 32)
                  (->> (drop 2)))]
    cells))
