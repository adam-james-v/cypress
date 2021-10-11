(ns cypress.polygen
  (:require [clojure.string :as str]
            [reagent.dom :as rdom]
            [reagent.core :as r]
            [svg-clj.elements :as el]
            [svg-clj.transforms :as tf]
            [svg-clj.composites :as comp :refer [svg]]
            [sci.core :as sci]
            [cypress.main :as cyp]))

(def state
  (r/atom
   {:width  {:input-type :slider :value 400 :min 100 :max 2000 :step 5}
    :height {:input-type :slider :value 400 :min 100 :max 2000 :step 5}
    :n-points {:input-type :slider :value 20 :min 10 :max 120}
    :n-quads {:input-type :slider :value 4 :min 0 :max 80}}))

(def render
  (r/atom
   (el/g
    (-> (el/circle 150)
        (tf/style {:fill "none" :stroke "black" :stroke-width "3px"}))
    (-> (el/rect 400 400)
        (tf/style {:fill "white"})))))

(defn button
  [label f]
  [:input {:type "button"
           :on-click f
           :value label
           :style {:width "100px"
                   :margin "4px"}}])

(defmulti control
  (fn [[_ {:keys [input-type]}]]
    input-type))

(defmethod control :slider
  [[param {:keys [value min max step] :as ctrl}]]
  [:div {:key param :style {:margin-bottom "3px"}}
   [:span {:style {:font-weight "bold"
                   :display "inline-block"
                   :width "55px"
                   :text-align "right"}} (name param)]
   [:input {:type "range" :value value :min min :max max :step step
            :style {:width "200px"
                    :padding 0
                    :vertical-align "middle"
                    :margin "0px 10px"}
            :on-change
            (fn [e]
              (let [new-value (js/parseInt (.. e -target -value))
                    new-ctrl (assoc ctrl :value new-value)]
                (swap! state
                       (fn [data]
                         (-> data
                             (assoc param new-ctrl))))))}]
   [:span value]])

(defmethod control :dropdown
  [[param {:keys [value] :as ctrl}]]
  identity)

(defmethod control :default
  [[param {:keys [value min max step] :as ctrl}]]
  identity)

(defn polygen
  [{:keys [width height n-points n-quads] :as state}]
  (-> (cyp/gen-data
       (cyp/random-pts (:value width) (:value height) (:value n-points))
       {:bg "none"
        :fg "slategray"
        :quads-n (:value n-quads)})
      :render))

(defn to-json [v] (.stringify js/JSON v))

(defn download-object
  [value export-name]
  (let [data-blob (js/Blob. #js [value] #js {:type "image/svg+xml"})
        link (.createElement js/document "a")]
    (set! (.-href link) (.createObjectURL js/URL data-blob))
    (.setAttribute link "download" export-name)
    (.appendChild (.-body js/document) link)
    (.click link)
    (.removeChild (.-body js/document) link)))

(def about
  [:<>
   [:p "Polygen is a generative art piece inspired by some prints I saw at IKEA. They're nice abstract linework pieces, with obvious randomness, but a simple underlying structure, which is perfect for making some art."]
   [:p "The name Polygen comes from the fact that it generates polygons. That's it, really."]
   [:p "This interface is just the first day pass, It will improve over the next few days as I add more sketches to the project."]])

(defn doc []
  [:<>
   [:h1 {:style {:width "100%"
                 :text-align "center"}} "Cypress:polygen"]
   about
   (conj
    (into [:div {:style
                 {:max-width "400px"
                  :margin "0 auto"
                  :padding 10
                  :display "flex"
                  :flex-direction "column"
                  :justify-content "center"}}
           [:h3 "Parameters"]]
          (for [param @state] [control param]))
   [button "Run" #(reset! render (polygen @state))])
   [:div
    {:id "result"
     :style {:display "grid"
             :width "max-content"
             :height "max-content"
             :margin "24px"
             :background "white"
             :box-shadow "rgba(0, 0, 0, 0.24) 0px 3px 8px"}} [svg @render]]
   [:div
    [button "Save"
     #(download-object
       (.-innerHTML (js/document.getElementById "result"))
       "polygen.svg")]]])

(defn mount [app]
  (rdom/render [app] (js/document.getElementById "app")))

(mount doc)
(defn ^:after-load re-render [] (mount doc))
(defonce go (do (mount doc) true))
