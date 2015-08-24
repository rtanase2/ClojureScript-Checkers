(ns checkers.board
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [checkers.resources :as res]
            [checkers.output :as cout]
            [clojure.string :refer [replace]]))

(enable-console-print!)

; == Notes ==============================================
; Board pieces are defined in the checkers.css file.  The
; currently defined pieces are:
;     :red-piece
;     :black-piece
;     :prom-red-piece
;     :prom-black-piece
;     :selected-red-piece
;     :selected-black-piece
;     :selected-prom-red-piece
;     :selected-prom-black-piece
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

; Updates the key k to contain the value v
(defn update-board-info! [k v]
  (swap! res/board-info assoc k v))

; Takes in the currently selected position and
; the corner position and figures out which
; corner is in question. Returns the keyword
; associated with the selected corner
(defn which-corner? [main-pos corner-pos]
  (let [curr-row (Math/ceil (/ main-pos 4))
        row-odd? (odd? curr-row)
        up-left (if row-odd? (- main-pos 4)
                  (- main-pos 5))
        up-right (if row-odd? (- main-pos 3)
                   (- main-pos 4))
        down-left (if row-odd? (+ main-pos 4)
                    (+ main-pos 3))
        down-right (if row-odd? (+ main-pos 5)
                     (+ main-pos 4))]
    (cond
     (= corner-pos up-left) :up-left
     (= corner-pos up-right) :up-right
     (= corner-pos down-left) :down-left
     (= corner-pos down-right) :down-right)))

; Calculates the end point of a skip starting at
; pos and jumping to the specified corner. Returns
; the end point of the skip.
(defn calc-skip-pos [pos corner]
  (cond
   (= corner :up-left) (- pos 9)
   (= corner :up-right) (- pos 7)
   (= corner :down-left) (+ pos 7)
   (= corner :down-right) (+ pos 9)))

; Takes in the start and end positions of a skip to
; be performed and finds the middle piece. Returns
; position of the skipped piece.
(defn find-skipped-piece [curr-selected clicked-pos]
  (let [offset (- clicked-pos curr-selected)
        direction (cond
                   (= offset -7) :up-right
                   (= offset -9) :up-left
                   (= offset 9) :down-right
                   (= offset 7) :down-left)
        curr-row (Math/ceil (/ curr-selected 4))
        row-odd? (odd? curr-row)]
    (cond
     (= direction :up-right) (if row-odd?
                               (- curr-selected 3)
                               (- curr-selected 4))
     (= direction :down-right) (if row-odd?
                                 (+ curr-selected 5)
                                 (+ curr-selected 4))
     (= direction :up-left) (if row-odd?
                              (- curr-selected 4)
                              (- curr-selected 5))
     (= direction :down-left) (if row-odd?
                                (+ curr-selected 4)
                                (+ curr-selected 3)))))

; Gets piece-pos which is an int and pos-skips-corners
; which is a vector of keywords containing a combintaion
; of :up-right, :up-left, :down-right and :down-left.
; Returns vector of possible skips and eliminates
; impossible ones.
(defn skips-possible? [piece-pos pos-skip-corners]
  (let [sec-top-row 2
        sec-bottom-row 7
        curr-row (Math/ceil (/ piece-pos 4))
        top-edge? (or (= curr-row top-row)
                      (= curr-row sec-top-row))
        bottom-edge? (or (= curr-row bottom-row)
                         (= curr-row sec-bottom-row))
        right-edge? (= (mod piece-pos 4) 0)
        left-edge? (= (mod piece-pos 4) 1)
        filter-val (fn [k vect]
                     (filter #(= k %) vect))]
    (cond
     (and bottom-edge? left-edge?)
     (filter-val :up-right pos-skip-corners)
     (and bottom-edge? right-edge?)
     (filter-val :up-left pos-skip-corners)
     (and top-edge? left-edge?)
     (filter-val :down-right pos-skip-corners)
     (and top-edge? right-edge?)
     (filter-val :down-left pos-skip-corners)
     bottom-edge? (concat (filter-val :up-right pos-skip-corners)
                          (filter-val :up-left pos-skip-corners))
     top-edge?(concat (filter-val :down-right pos-skip-corners)
                      (filter-val :down-left pos-skip-corners))
     right-edge? (concat (filter-val :down-left pos-skip-corners)
                         (filter-val :up-left pos-skip-corners))
     left-edge? (concat (filter-val :up-right pos-skip-corners)
                        (filter-val :down-right pos-skip-corners))
     :else pos-skip-corners)))

; Takes a vector of positions and returns the pieces
; that are of the opposite color
(defn find-opposing-neighbors [neighbors]
  (let [curr-color (name (@res/board-info :curr-color))
        other-color (if (= curr-color "black")
                      "red" "black")]
    (filter (fn [pos]
              (or (= (keyword (str "prom-"
                                   other-color
                                   "-piece"))
                     (@board pos))
                  (= (keyword (str other-color
                                   "-piece"))
                     (@board pos))))
            neighbors)))

; Takes a vector of positions and returns a vector
; of positions that are empty
(defn find-empty-neighbors [neighbors]
  (filter #(= :empty-piece (@board %)) neighbors))

; Takes in the current piece position and a vector of
; its adjacent neighbors. It then analyzes the neighbors
; and removes impossible moves and adds skip options.
; Returns vector of all possible moves for current piece.
(defn add-skips [piece-pos neighbors]
  (let [pos-skips (find-opposing-neighbors neighbors)
        empty-neighbors (find-empty-neighbors neighbors)]
    (as-> pos-skips skips-vec
          ; Takes vector of positions and returns
          ; an array of keywords saying which corner
          ; that position is compared to the piece-pos
          (map
           #(which-corner? piece-pos %)
           skips-vec)
          ; Removes invalid skips. Skips that cannot
          ; exist based off of position. For example,
          ; the piece is on the left edge, so no up-left
          ; or down-left skips can exist
          (skips-possible? piece-pos skips-vec)
          ; Takes the keyword vector and computes
          ; the skip positions
          (map
           #(calc-skip-pos piece-pos %)
           skips-vec)
          ; Filters out all positions that are not
          ; type :empty-piece
          (concat empty-neighbors
                  (filter
                   #(= (@board %) :empty-piece)
                   skips-vec)))))

; Given a board position, return the position of neighbors
; [NOTE:] Challengee should investigate memoization of
;         this function.
;
; I am thinking about making a board atom that I can update
; only the old spot, new spot and new spot's neighbors after
; a move is successfully moved. Maybe be too hard but will
; try that.
(defn compute-pos-neighbors [pos]
  (let [piece-type (name (@board pos))
        curr-row (Math/ceil (/ pos 4))
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
                    (+ pos 3))
        down-right (if row-odd? (+ pos 5)
                     (+ pos 4))
        neighbors (remove nil?
                          (flatten
                           ; Determine which upper pieces to include
                           [(if (not top-row?)
                              (if row-even?
                                [(if (not left-edge?)
                                   up-left)
                                 up-right]
                                [(if (not right-edge?)
                                   up-right)
                                 up-left]))
                            ; Determine which lower pieces to include
                            (if (not bottom-row?)
                              (if row-odd?
                                [(if (not right-edge?)
                                   down-right)
                                 down-left]
                                [(if (not left-edge?)
                                   down-left)
                                 down-right]))]))
        empty-neighbors (find-empty-neighbors neighbors)
        valid-empty-neighbors (if (re-find #"prom"
                                           piece-type)
                                empty-neighbors
                                (filter #((if
                                            (= (@res/board-info :curr-color)
                                               :red)
                                            < >)
                                          pos %) empty-neighbors))
        neighbors-with-skips (add-skips pos neighbors)
        valid-neighbors-with-skips (if (re-find #"prom"
                                                piece-type)
                                     neighbors-with-skips
                                     (filter #((if
                                                 (= (@res/board-info :curr-color)
                                                    :red)
                                                 < >)
                                               pos %) neighbors-with-skips))]
    ; If the empty-neighbors and the neighbor-with-skips
    ; are not equal, then there must be a skip
    (if (and (not (= valid-empty-neighbors
                     valid-neighbors-with-skips))
             (re-find (re-pattern
                       (name
                        (@res/board-info :curr-color)))
                      piece-type))
      (do
        (update-board-info! :skip-available? true)))
    neighbors-with-skips))

; Computes neighbors for every piece in pieces-vec.
; pieces-vec is a vector of all positions you want
; neighbors of.
(defn compute-neighbor-positions [pieces-vec]
  (map (fn [pos] {pos (compute-pos-neighbors pos)})
       pieces-vec))

; Returns a set of all the valid pieces that can be
; selected at this time based off of the current
; color in board-info
(defn get-valid-piece-types []
  (let [color (name (@res/board-info :curr-color))]
    (set
     (map keyword [(str color "-piece")
                   (str "prom-" color "-piece")
                   (str "selected-" color "-piece")
                   (str "selected-prom-" color "piece")]))))

; Takes in a keyword piece and returns the keyword type
; of the piece after a click has occurred.
; :black-piece => :selected-black-piece
; :selected-black-piece => :black-piece
(defn update-piece-type [piece]
  (let [str-piece (name piece)
        selected? (re-find #"selected" str-piece)]
    (if selected?
      (keyword (replace str-piece #"selected-" ""))
      (keyword (str "selected-" str-piece)))))

; Promotes the current piece to king. Piece is the keyword
; type of piece in question. If the piece type is already
; kinged, then return piece.
(defn prom-piece [piece]
  (cond
   (= piece :red-piece) :prom-red-piece
   (= piece :black-piece) :prom-black-piece
   (= piece :selected-red-piece) :selected-prom-red-piece
   (= piece :selected-black-piece) :selected-prom-black-piece
   :else piece))

; Adds a board command to board-commands with :command command,
; :position position and :piece piece
(defn add-board-command [command, position, piece]
  (put! board-commands
        {:command command
         :position position
         :piece piece}))

; Determines if the piece at pos should be kinged or not
; Returns true if it should be kinged and false if not.
; Pos is the square in question
(defn change-to-prom? [pos]
  (let [curr-row (Math/ceil (/ pos 4))
        curr-color (@res/board-info :curr-color)]
    ; If the current color is red and the pos is
    ; in the bottom row or if the current color
    ; is black and the pos is in the top row,
    ; return true. Else return false
    (if (or (and (= bottom-row curr-row) (= curr-color :red))
            (and (= top-row curr-row) (= curr-color :black)))
      true
      false)))

; Takes in the click event and updates the board based on
; the clicked position and the board-info
(defn move-piece [event]
  (let [clicked-pos (:position event)
        clicked-piece-type (@board clicked-pos)
        curr-selected (@res/board-info :curr-selected)
        pos-neighbors (set (compute-pos-neighbors
                            curr-selected))
        curr-selected-type (name (@board curr-selected))
        player-color (@res/board-info :curr-color)
        ; If it has prom in the name, it can move both ways
        ; If it doesn't have prom in the name it can only move
        ; forward.
        ;
        ; Forward for red is if the neighbor space is
        ; greater than the currently selected piece
        ; Forward for black is if the neighbor space is
        ; less than the currently selected piece)
        valid-neighbors (if (re-find #"prom"
                                     curr-selected-type)
                          pos-neighbors
                          (filter #((if (= player-color :red)
                                      < >)
                                    curr-selected %)
                                  pos-neighbors))]
    ; Check if the clicked piece is empty.
    (if (= clicked-piece-type :empty-piece)
      ; If it is empty, see if it is a neighbor
      (do
        (if (get (set valid-neighbors) clicked-pos)
          ; If there is valid  neighbor AND the players chose
          ; a skipping move
          (if (@res/board-info :skip-available?)
            (if (> (Math/abs (- clicked-pos curr-selected)) 5)
              ; Then update the board accordingly
              (let [skipped-piece (find-skipped-piece
                                   curr-selected
                                   clicked-pos)]
                (do
                  (add-board-command
                   :update-board-position
                   skipped-piece
                   :empty-piece)
                  (add-board-command
                   :update-board-position
                   curr-selected
                   :empty-piece)
                  ; If the piece is in a place where it could
                  ; be promoted to king
                  (if (change-to-prom? clicked-pos)
                    ; If the piece is in a place where it can
                    ; be promoted to king
                    (do
                      (println "skipped prom")
                      (println (@board curr-selected))
                      (println (update-piece-type (@board curr-selected)))
                      (add-board-command
                       :update-board-position
                       clicked-pos
                       (prom-piece (@board curr-selected))))

                    ; Else move the piece and unselect it
                    (add-board-command
                     :update-board-position
                     clicked-pos
                     (@board curr-selected)))
                  (update-board-info! :last-move-a-skip? true)
                  (update-board-info! :curr-selected clicked-pos)
                  (update-board-info! :skip-available? false)
                  (add-board-command :check-for-jump
                                     clicked-pos
                                     nil)))
              ; Else if, there is a skip, but they chose a
              ; non-skipping move, then don't let them move
              ; and print and error message
              (cout/update-system-out-text "A skip is available so, you must skip."))
            ; If no skips are available, then update the board normally
            (do
              (add-board-command
               :update-board-position
               curr-selected
               :empty-piece)
              ; If the piece is in a place where it could
              ; be promoted to king
              (if (change-to-prom? clicked-pos)
                ; If the piece is in a pace where it can
                ; be promoted to king
                (add-board-command
                 :update-board-position
                 clicked-pos
                 (prom-piece (update-piece-type (@board curr-selected))))
                ; Else move the piece and unselect it
                (add-board-command
                 :update-board-position
                 clicked-pos
                 (update-piece-type (@board curr-selected))))
              (update-board-info! :valid-selection? false)
              (update-board-info! :curr-selected nil)
              (update-board-info! :curr-color
                                  (if (= player-color :red)
                                    :black
                                    :red))
              (update-board-info! :skip-available? false)))
          ; Else, it is not a neighbor and print saying it
          ; is not a valid move
          (cout/update-system-out-text
           (str "You cannot move the currently selected "
                "piece there. Please try again."))))
      ; Else, it is occupied, so it is not a valid move
      ; so print and error message.
      (cout/update-system-out-text
       "Cannot move there. Please try again."))))

; Ensure that clicked piece is the correct color and has
; valid moves available to it. If not, print a message
; to tell the player why it did not work
(defn validate-clicked-piece [event]
  (let [curr-selected (@res/board-info :curr-selected)
        curr-selected-type (@board curr-selected)
        clicked-pos (:position event)
        clicked-piece-type (@board clicked-pos)
        current-player-color (name (@res/board-info
                                    :curr-color))
        valid-selection? (@res/board-info :valid-selection?)
        pos-neighbors (compute-pos-neighbors clicked-pos)
        valid-neighbors (if (re-find #"prom"
                                     (name clicked-piece-type))
                          pos-neighbors
                          (filter #((if
                                      (= current-player-color
                                         "red")
                                      < >)
                                    clicked-pos %)
                                  pos-neighbors))]
    ; If the piece clicked is from the correct player
    (if (get (get-valid-piece-types) clicked-piece-type)
      ; If the piece that is clicked is the current
      ; selected piece
      (if (= curr-selected clicked-pos)
        ; Then unselect it and update res/board-info to
        ; show there is no valid selection and no piece
        ; currently selected
        (do
          (add-board-command
           :update-board-position
           curr-selected
           (update-piece-type curr-selected-type))
          (update-board-info! :valid-selection? false)
          (update-board-info! :curr-selected nil))
        ; Else, check if the piece has possible moves
        (if (some #(= :empty-piece %)
                  (map #(@board %)
                       valid-neighbors))
          ; If there are valid moves available, update
          ; the board by unselecting old selected piece,
          ; selecting the clicked piece and showing that
          ; valid selection is chosen and which space
          ; it is
          (do
            (put! board-commands
                  {:command :update-board-position
                   :position clicked-pos
                   :piece (update-piece-type
                           clicked-piece-type)})
            (if (@res/board-info :valid-selection?)
              (do
                (add-board-command
                 :update-board-position
                 curr-selected
                 (update-piece-type curr-selected-type))))
            (update-board-info! :valid-selection? true)
            (update-board-info! :curr-selected clicked-pos))
          ; Else, print an error message stating that
          ; there are no valid moves for that piece and
          ; to try again
          (do (cout/update-system-out-text
               (str "Selected piece does not have any "
                    "available moves. Please select a "
                    "different piece."))
            (if (@res/board-info :valid-selection?)
              (do
                (add-board-command
                 :update-board-position
                 curr-selected
                 (update-piece-type curr-selected-type))))
            (update-board-info! :valid-selection? false)
            (update-board-info! :curr-selected nil))))
      ; Else, check if there is a currently selected piece
      (if valid-selection?
        ; If there is, move the piece to the spot if valid
        (move-piece event)
        ; Else, print an error message stating which color
        ; piece a valid choice, unselect current selected
        ; piece and update the board-info map's
        ; valid-selection? and curr-selected values
        (do
          (cout/update-system-out-text
           (str "Invalid piece. Please choose a "
                current-player-color
                " piece."))
          (if (@res/board-info :valid-selection?)
              (add-board-command
               :update-board-position
               curr-selected
               (update-piece-type curr-selected-type))))
          (update-board-info! :valid-selection? false)
          (update-board-info! :curr-selected nil))))))

; Takes in a keyword piece-type and returns vector
; of all positions with that type that are in
; pos-vec
(defn get-all-pos-of-color [piece-type pos-vec]
  (filter #(= (@board %)
              piece-type) pos-vec))

(defn set-skips-available []
  (let [all-squares (range 1 33)
        curr-piece-type (name (@res/board-info :curr-color))
        normal-pieces (get-all-pos-of-color
                       (keyword (str curr-piece-type
                                     "-piece"))
                       all-squares)
        prom-pieces (get-all-pos-of-color
                     (keyword (str "prom-"
                                   curr-piece-type
                                   "-piece"))
                     all-squares)]
    (doall
     (compute-neighbor-positions (concat normal-pieces
                                         prom-pieces)))))

; == Concurrent Processes =================================
; this concurrent process reacts to board click events --
; at present, it sets the board position clicked to contain
; a black piece by sending a command to the board-commands
; channel
(go (while true
      (let [event (<! board-events)]
        (cout/clear-system-out)
        (set-skips-available)
        (println (@res/board-info :skip-available?))
        (validate-clicked-piece event))))

; this concurrent process receives board command messages
; and executes on them.  at present, the only thing it does
; is sets the desired game position to the desired piece
(go (while true
      (let [command (<! board-commands)
            command-type (command :command)
            curr-selected (@res/board-info :curr-selected)]
        (println command-type)
        (println (:position command))
        (println (:piece command))
        (cond
         (= command-type :update-board-position)
         (swap! board assoc (:position command)
                (:piece command))
         (= command-type :check-for-jump)
         (do
           (compute-pos-neighbors (@res/board-info :curr-selected))
           (if (not (@res/board-info :skip-available?))
             ; Update things.
             (do
               (add-board-command :update-board-position
                                  curr-selected
                                  (update-piece-type
                                   (@board curr-selected)))
               (update-board-info! :curr-color
                                   (if (= (@res/board-info :curr-color) :red)
                                     :black
                                     :red))
               (update-board-info! :curr-selected nil)
               (update-board-info! :valid-selection? false)
               (update-board-info! :last-move-a-skip? false))))))))
