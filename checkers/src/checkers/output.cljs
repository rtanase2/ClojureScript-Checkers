(ns checkers.output
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [checkers.resources :refer [app-state]]))

(enable-console-print!)

; == Update Move Description Text =======================
; Initializes the text displayed near the board
(defn init-text []
   (om/root
    (fn [data owner]
      (om/component (dom/h1 #js {:className "title"} (:header data))))
    app-state
    {:target (. js/document (getElementById "title"))})

  (om/root
   (fn [data owner]
     (om/component (dom/h2 #js {:className "plain-text"} (:system-out data))))
   app-state
   {:target (. js/document (getElementById "system-output"))})

  (om/root
    (fn [data owner]
      (om/component (dom/p #js {:className "plain-text"} (:instructions data))))
    app-state
    {:target (. js/document (getElementById "instructions"))})

  (om/root
    (fn [data owner]
      (om/component (dom/p #js {:className "plain-text"} (:rules data))))
    app-state
    {:target (. js/document (getElementById "rules"))}))

; Calls the correct functions to properly update the
; system-output text
(defn update-system-out-text [output-text]
  ; Updates system-output text to output-text
  (swap! app-state assoc :system-out output-text))

(defn clear-system-out []
  (update-system-out-text ""))
