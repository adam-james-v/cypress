(ns cypress.main
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.composites :as c :refer [svg]]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            [svg-clj.parametric :as p]
            [svg-clj.utils :as utils]))

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
