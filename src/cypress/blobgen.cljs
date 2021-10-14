(ns cypress.blobgen
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
   {}
   #_{:t {:input-type :slider :value 0 :min 0 :max 100 :step 4}}))

(def render
  (r/atom ((cyp/amoeba) 0.5)))

(def about
  [:<>
   [:p "Blobgen is an experiment in creating organic shapes."]
   [:p "This first pass at blob generation works reasonably well and produces things that I think look like amoebas. Neat!"]
   [:p "To throw some extra stuff on top of the blob gen, I came up with some edge drawings to add some visual flair."]])

(defn doc []
  [:<>
   [:h1 {:style {:width "100%"
                 :text-align "center"}} "Cypress:blobgen"]
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
    [ui/button "Run" #(reset! render ((cyp/amoeba) 0.5))])
   [:div
    {:id "result"
     :style {:display "grid"
             :width "max-content"
             :height "max-content"
             :margin "24px"
             :padding "10px"
             :background "white"
             :box-shadow "rgba(0, 0, 0, 0.24) 0px 3px 8px"}}
    [svg @render] #_(-> @render
        (tf/translate [250 250])
        [svg 500 500])]
   [:div
    [ui/button "Save"
     #(ui/download-object
       (.-innerHTML (js/document.getElementById "result"))
       "blobgen.svg")]]])

(defn mount [app]
  (rdom/render [app] (js/document.getElementById "app")))

(mount doc)
(defn ^:after-load re-render [] (mount doc))
(defonce go (do (mount doc) true))
