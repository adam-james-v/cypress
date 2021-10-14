(ns cypress.polygen
  (:require [clojure.string :as str]
            [reagent.dom :as rdom]
            [reagent.core :as r]
            [svg-clj.elements :as el]
            [svg-clj.transforms :as tf]
            [svg-clj.composites :as comp :refer [svg]]
            [cypress.main :as cyp]
            [cypress.ui :as ui]))

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

(defn polygen
  [{:keys [width height n-points n-quads] :as state}]
  (-> (cyp/gen-data
       (cyp/random-pts (:value width) (:value height) (:value n-points))
       {:bg "none"
        :fg "slategray"
        :quads-n (:value n-quads)})
      :render))

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
          (for [param @state] [ui/control state param]))
   [ui/button "Run" #(reset! render (polygen @state))])
   [:div
    {:id "result"
     :style {:display "grid"
             :width "max-content"
             :height "max-content"
             :margin "24px"
             :background "white"
             :box-shadow "rgba(0, 0, 0, 0.24) 0px 3px 8px"}} [svg @render]]
   [:div
    [ui/button "Save"
     #(ui/download-object
       (.-innerHTML (js/document.getElementById "result"))
       "polygen.svg")]]])

(defn mount [app]
  (rdom/render [app] (js/document.getElementById "app")))

(mount doc)
(defn ^:after-load re-render [] (mount doc))
(defonce go (do (mount doc) true))
