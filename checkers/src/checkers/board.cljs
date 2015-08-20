(ns checkers.board
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [checkers.resources :as res]
            [checkers.output :as cout]))

(enable-console-print!)

; == Notes ==============================================
; Board pieces are defined in the checkers.css file.  The
; currently defined pieces are:
;     :red-piece
;     :black-piece
;     :prom-red-piece
;     :prom-black-piece
;     :empty
;
; The board is laid out as a 32 element map, one element
; for each position.  It is stored in an atom, and bound
; to the UI.  Any update of the atom will cause an UI
; refresh to reflect the current board state.
;
; core.async is used to implement CSP (Communicating
; Sequential Proceses), and channels are used to report
; user interaction events, as well as changing the board
; state.

; ===Channels ===========================================
; the board generates events on this channel
;     {:event :event-symbol
;      :position <int>}
(def board-events (chan))

; the board receives commands to manipulate its state
;     {:command :command-symbol
;      :position <integer>
;      :piece :piece-symbol}

(def board-commands (chan))

; for other processes to acquire the board state atom
;     (atom (create-board))
(def board-state (chan))

; == Board State ==========================================
; initialize a board, where positions are indexed 1-32.
; each position is an atom containing the symbol of the
; piece in it.
(defn create-board []
  (atom
   (apply sorted-map
          (flatten
           (map-indexed (fn [i v] (vector (inc i) v))
                        (flatten
                         [(repeat 12 :red-piece)
                          (repeat 8 :empty-piece)
                          (repeat 12 :black-piece)]))))))

; instantiate our game board state, initializing our
; board with starting pieces
(def board (create-board))

; === Utility Functions =================================
; positional constants
(def top-row 1)
(def bottom-row 8)
(def curr-piece (atom :black-piece))

; given a board position, return the position of neighbors
; [NOTE:] Challengee should investigate memoization of
;         this function.
(defn compute-pos-neighbors [pos]
  (let [curr-row (Math/ceil (/ pos 4))
        row-odd? (odd? curr-row)
        row-even? (not row-odd?)
        top-row? (= curr-row top-row)
        bottom-row? (= curr-row bottom-row)
        right-edge? (= (mod pos 4) 0)
        left-edge? (= (mod pos 4) 1)
        up-left (if row-odd? (- pos 4)
                  (- pos 5))
        up-right (if row-odd? (- pos 3)
                   (- pos 4))
        down-left (if row-odd? (+ pos 4)
                    (+ pos 5))
        down-right (if row-odd? (+ pos 3)
                     (+ pos 4))]
    (remove nil?
            (flatten
             [(if (not top-row?)
                (if row-even?
                  [up-left up-right]
                  [(if (not left-edge?)
                     up-left)
                   (if (not right-edge?)
                     up-right)]))
              (if (not bottom-row?)
                (if row-odd?
                  [down-left down-right]
                  [(if (not left-edge?)
                     down-left)
                   (if (not right-edge?)
                     down-right)]))]))))

; compute neighbors for every board position
(defn compute-neighbor-positions []
  (map (fn [pos] {pos (compute-pos-neighbors pos)})
       (range 1 33)))

(defn get-valid-piece-types []
  (if (= (@res/board-info :curr-color) :red)
    #{:red-piece, :selected-red-piece, :prom-red-piece, :selected-prom-red-piece}
    #{:black-piece, :selected-black-piece, :prom-black-piece, :selected-prom-black-piece}))

(defn determine-piece [piece]
  (cond
   (= piece :red-piece) :selected-red-piece
   (= piece :black-piece) :selected-black-piece
   (= piece :prom-red-piece) :selected-prom-red-piece
   (= piece :prom-black-piece) :selected-prom-red-piece
   (= piece :selected-red-piece) :red-piece
   (= piece :selected-black-piece) :black-piece
   (= piece :selected-prom-red-piece) :prom-red-piece
   (= piece :selected-prom-black-piece) :prom-red-piece))

; == Concurrent Processes =================================
; this concurrent process reacts to board click events --
; at present, it sets the board position clicked to contain
; a black piece by sending a command to the board-commands
; channel

; PUT PIECE LOGIC HERE
;
; SOMEHOW FIGURE OUT HOW TO CHANGE TO ONLY ONCE ACTIVE PIECE AT A TIME
; IT SHOULDN'T BE THIS HARD.
;
; TODO: Remove valid-selection when its not valid along with curr-selected
(go (while true
      (let [event (<! board-events)]
        (cout/system-out-text-delegator "")
        (if (get (get-valid-piece-types) (@board (:position event)))
          (if (= (@res/board-info :curr-selected) (:position event))
            (do
              (println "already selected!!")
              (put! board-commands
                    {:command :update-board-position
                     :position (@res/board-info :curr-selected)
                     :piece (determine-piece (@board (@res/board-info :curr-selected)))})
              (swap! res/board-info assoc :valid-selection false)
              (swap! res/board-info assoc :curr-selected nil))
            (do
              (println "not already selected")
              (put! board-commands
                    {:command :update-board-position
                     :position (:position event)
                     :piece (determine-piece (@board (:position event)))})
              (if (@res/board-info :valid-selection)
                (put! board-commands
                    {:command :update-board-position
                     :position (@res/board-info :curr-selected)
                     :piece (determine-piece (@board (@res/board-info :curr-selected)))}))
              (swap! res/board-info assoc :valid-selection true)
              (swap! res/board-info assoc :curr-selected (:position event))
              (println (@res/board-info :valid-selection))
              (println (@res/board-info :curr-selected))))

            (cout/system-out-text-delegator (str "Invalid piece. Please choose a " (name (@res/board-info :curr-color)) " piece"))))))

    ; this concurrent process receives board command messages
    ; and executes on them.  at present, the only thing it does
    ; is sets the desired game position to the desired piece
    (go (while true
          (let [command (<! board-commands)]
            (swap! board assoc (:position command)
                   (:piece command)))
          (println "Board Commands called!!!")))
