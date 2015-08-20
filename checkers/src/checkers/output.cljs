(ns checkers.output
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [checkers.resources :refer [app-state]]))

(enable-console-print!)

(defonce app-state
  (atom {:header "Welcome to Rachelle's ClojureScript Checkers!"
         :instructions (str "You are the red player. To move, click "
                            "the piece you'd like to move and then "
                            "click the square you'd like to move it to.")
         :system-out ""}))

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
    {:target (. js/document (getElementById "instructions"))}))

; Calls the correct functions to properly update the
; system-output text
(defn system-out-text-delegator []
  ; See what previous text was to determine what to change
  ; text to.
  (let
    [board-pos (cond
                  (= 2 1) "hello"
                  (= 3 2) "hi"
                  :else "blarg")]
      (swap! app-state assoc :system-out (str "You have clicked " (str board-pos)))))
