(ns checkers.resources
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state
  (atom {:header "Welcome to Rachelle's ClojureScript Checkers!"
         :instructions (str "You are the black player. To move, click "
                            "the piece you'd like to move and then "
                            "click the square you'd like to move it to.")
         :system-out ""}))

(defonce board-info
  (atom {:curr-color :black
         :valid-selection false
         :curr-selected nil}))
