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

; Returns keyword of other color
; Red -> Black, Black -> Red
(defn get-opposite-color []
  (if (= (@res/board-info :curr-color) :red)
    :black
    :red))

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
        filter-on-psc (fn [k]
                        (filter #(= k %) pos-skip-corners))]
    (cond
     (and bottom-edge? left-edge?) (filter-on-psc :up-right)
     (and bottom-edge? right-edge?) (filter-on-psc :up-left)
     (and top-edge? left-edge?) (filter-on-psc :down-right)
     (and top-edge? right-edge?) (filter-on-psc :down-left)
     bottom-edge? (concat (filter-on-psc :up-right)
                          (filter-on-psc :up-left))
     top-edge?(concat (filter-on-psc :down-right)
                      (filter-on-psc :down-left))
     right-edge? (concat (filter-on-psc :down-left)
                         (filter-on-psc :up-left))
     left-edge? (concat (filter-on-psc :up-right)
                        (filter-on-psc :down-right))
     :else pos-skip-corners)))

; Takes a vector of positions and returns the pieces
; that are of the opposite color
(defn find-opposing-neighbors [neighbors]
  (let [curr-color (name (@res/board-info :curr-color))
        other-color (name (get-opposite-color))]
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

; Finds all pieces that are adjacent to pos
(defn find-all-neighbors [pos]
  (let [curr-row (Math/ceil (/ pos 4))
        row-odd? (odd? curr-row)
        row-even? (not row-odd?)
        top-row? (= curr-row top-row)
        bottom-row? (= curr-row bottom-row)
        right-edge? (= (mod pos 4) 0)
        left-edge? (= (mod pos 4) 1)
        up-left (if row-odd? (- pos 4) (- pos 5))
        up-right (if row-odd? (- pos 3) (- pos 4))
        down-left (if row-odd? (+ pos 4) (+ pos 3))
        down-right (if row-odd? (+ pos 5) (+ pos 4))]
    (remove nil?
            (flatten
             ; Determine which upper pieces to include
             [(if (not top-row?)
                (if row-even?
                  [(if (not left-edge?) up-left)
                   up-right]
                  [(if (not right-edge?) up-right)
                   up-left]))
              ; Determine which lower pieces to include
              (if (not bottom-row?)
                (if row-odd?
                  [(if (not right-edge?) down-right)
                   down-left]
                  [(if (not left-edge?) down-left)
                   down-right]))]))))

; Finds all pieces that are valid neighbors
; from pos-vect based off of piece type
; aka prom or not and returns vector
; of vaild neighbors
(defn get-valid-neighbors [pos pos-vect]
  (let [piece-type (name (@board pos))]
    (if (re-find #"prom" piece-type)
      pos-vect
      (filter
       #((if (= (@res/board-info :curr-color) :red)
           < >) pos %) pos-vect))))

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
        neighbors (find-all-neighbors pos)
        empty-neighbors (find-empty-neighbors neighbors)
        valid-empty-neighbors (get-valid-neighbors
                               pos empty-neighbors)
        neighbors-with-skips (add-skips pos neighbors)
        valid-neighbors-with-skips (get-valid-neighbors
                                    pos
                                    neighbors-with-skips)]
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
    valid-neighbors-with-skips))

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
        curr-type (@board curr-selected)
        pos-neighbors (set (compute-pos-neighbors
                            curr-selected))
        valid-neighbors (get-valid-neighbors
                         curr-selected pos-neighbors)]
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
                  (add-board-command :update-board-position
                                     skipped-piece :empty-piece)
                  (add-board-command :update-board-position
                                     curr-selected :empty-piece)
                  ; If the piece is in a place where it could
                  ; be promoted to king
                  (if (change-to-prom? clicked-pos)
                    ; If the piece is in a place where it can
                    ; be promoted to king
                    (do
                      (add-board-command :update-board-position
                                         clicked-pos (prom-piece
                                                      curr-type)))
                    ; Else move the piece and unselect it
                    (add-board-command :update-board-position
                                       clicked-pos curr-type))
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
                 (prom-piece (update-piece-type curr-type)))
                ; Else move the piece and unselect it
                (add-board-command
                 :update-board-position
                 clicked-pos
                 (update-piece-type curr-type)))
              (update-board-info! :valid-selection? false)
              (update-board-info! :curr-selected nil)
              (update-board-info! :curr-color
                                  (get-opposite-color))
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
        valid-neighbors (get-valid-neighbors
                         clicked-pos pos-neighbors)]
    ; If the piece clicked is from the correct player
    (if (@res/board-info :last-move-a-skip?)
      ; If last move was a skip, don't let player change
      ; current piece then only allow skip
      (move-piece event)
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
              (add-board-command
               :update-board-position
               clicked-pos
               (update-piece-type
                clicked-piece-type))
              (if (@res/board-info :valid-selection?)
                (add-board-command
                 :update-board-position
                 curr-selected
                 (update-piece-type curr-selected-type)))
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
                (add-board-command
                 :update-board-position
                 curr-selected
                 (update-piece-type curr-selected-type)))
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
               (update-piece-type curr-selected-type)))
            (update-board-info! :valid-selection? false)
            (update-board-info! :curr-selected nil)))))))

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

; Prints a win message and makes the game end
(defn print-win-message [message]
  (do
    (cout/update-system-out-text message)
    (update-board-info! :game-over? true)))

(defn detect-win []
  (let [get-neighbors-of-type (fn [t]
                                (compute-neighbor-positions
                                 (filter #(= t (@board %))
                                         (range 1 33))))
        red-pieces (concat (get-neighbors-of-type
                            :red-piece)
                           (get-neighbors-of-type
                            :selected-red-piece))
        prom-red-pieces (concat (get-neighbors-of-type
                                 :prom-red-piece)
                                (get-neighbors-of-type
                                 :selected-prom-red-piece))
        black-pieces (concat (get-neighbors-of-type
                              :black-piece)
                             (get-neighbors-of-type
                              :selected-black-piece))
        prom-black-pieces (concat (get-neighbors-of-type
                                   :prom-black-piece)
                                  (get-neighbors-of-type
                                   :selected-prom-black-piece))
        all-red (do (concat red-pieces prom-red-pieces))
        all-black (do (concat black-pieces prom-black-pieces))
        num-black-moves (do (reduce + (map #(count (first (vals %))) all-black)))
        num-red-moves (do (reduce + (map #(count (first (vals %))) all-red)))
        curr-color (@res/board-info :curr-color)]
    (if (empty? all-red)
      (print-win-message "Black wins!"))
    (if (empty? all-black)
      (print-win-message "Red wins!"))
    (if (or (and (= :red curr-color) (zero? num-red-moves))
            (and (= :black curr-color) (zero? num-black-moves)))
      ; If current color has no moves, then determine winner
      ; by analyzing number of prom and normal pieces of
      ; each type
      (cond
       ; If there are more red then red wins
       (< (count all-red) (count all-black))
       (print-win-message "Black wins!")
       ; If there are more black then black wins
       (> (count all-red) (count all-black))
       (print-win-message "Red wins!")
       ; Else, there are equal number of pieces and the
       ; player with the most kings win. If number of
       ; kings is the same, then it is a tie
       :else
       (let [total-red-prom-pieces (count prom-red-pieces)
             total-black-prom-pieces (count prom-black-pieces)]
         (cond
          (< total-red-prom-pieces total-black-prom-pieces)
          (print-win-message "Black wins!")
          (> total-red-prom-pieces total-black-prom-pieces)
          (print-win-message "Red wins!")
          :else
          (print-win-message "It's a tie!")))))
    (update-board-info! :skip-available? false)))

; == Concurrent Processes =================================
; this concurrent process reacts to board click events --
; at present, it sets the board position clicked to contain
; a black piece by sending a command to the board-commands
; channel
(go (while true
      (let [event (<! board-events)]
        (if (not (@res/board-info :game-over?))
          (do
            (cout/clear-system-out)
            (set-skips-available)
            (validate-clicked-piece event)
            (add-board-command
             :check-win nil nil))))))

; this concurrent process receives board command messages
; and executes on them.  at present, the only thing it does
; is sets the desired game position to the desired piece
(go (while true
      (let [command (<! board-commands)
            command-type (command :command)
            curr-selected (@res/board-info :curr-selected)]
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
                                   (get-opposite-color))
               (update-board-info! :curr-selected nil)
               (update-board-info! :valid-selection? false)
               (update-board-info! :last-move-a-skip? false))))
         (= command-type :check-win)
         (detect-win)))))
