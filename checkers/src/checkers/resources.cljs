(ns checkers.resources
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state
  (atom {:header "Welcome to Rachelle's ClojureScript Checkers!"
         :rules (str "You are the black player, so you "
                     "go first. You must make a capture "
                     "if one is available. If multiple "
                     "are available, you can choose which"
                     " capture you make. You are awarded a"
                     " king if you make it to the row "
                     "farthest from you. Normal pieces can"
                     " only move diagonally forward and "
                     "kings can move diagonally forward and"
                     " backward. A game is over when a "
                     "player cannot make any moves more "
                     "moves, either because they have no "
                     "pieces left or their pieces are "
                     "trapped. The player who has less pieces "
                     "loses.")
         :instructions (str "To move a piece, click "
                            "the piece you would like to move "
                            "and click the space you would like"
                            " to move to.")
         :wins "Black: 0             Red: 0"
         :system-out ""}))

(defonce board-info
  (atom {:curr-color :black
         :valid-selection? false
         :curr-selected nil
         :skip-available? false
         :last-move-a-skip? false
         :game-over? false}))
