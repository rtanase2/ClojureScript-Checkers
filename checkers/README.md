Description:
====================================
Two player checkers game written in ClojureScript. Implements the US version of checkers. See Game Rules section
for more information on rules.

Features Implemented:
====================================
* Basic game rules
* Clicking to select, deselect and move pieces. Clikcing a selected piece and vice versa. When a piece is selected and you click an empty
  square, it will move the piece there
* Move checking. Validates that a move is valid before moving it.
* Two person checkers is supported

Game Rules:
====================================
* Black player starts
* If a skip is available, you must skip
* Normal pieces move diagonally forward, kinged pieces can move diagonally forward and back.
* Win if other person has no valid moves or has no pieces left
* Kinged (prom) pieces are awarded when they reach the row farthest away from the player.

How To Run:
====================================
1. Download files
2. Make sure Clojure, Leiningen and Java are installed on your machine
3. In terminal, get to directory and type 'lein cljsbuild once'
4. Open index.html in your web browser

Notes:
====================================
* To start a new game, you have to refresh the page
* Made assumption that submission guideline meant "compiles cleanly using `lein clean; lein cljsbuild once`", not just lein cljsbuild
* Breifly looked into allowing starting a new game after a game has been won but couldn't figure it out
* Looked into memoizing the finding neighbors function, but I did not notice a unresponsiveness without it, so I did not implement it
