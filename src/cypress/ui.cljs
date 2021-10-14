(ns cypress.ui
  (:require [clojure.string :as str]
            [reagent.dom :as rdom]
            [reagent.core :as r]))

(defn button
  [label f]
  [:input {:type "button"
           :on-click f
           :value label
           :style {:width "100px"
                   :margin "4px"}}])

(defmulti control
  (fn [_ [_ {:keys [input-type]}]]
    input-type))

(defmethod control :slider
  [state [param {:keys [value min max step] :as ctrl}]]
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
  [state [param {:keys [value] :as ctrl}]]
  identity)

(defmethod control :default
  [state [param {:keys [value min max step] :as ctrl}]]
  identity)

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
