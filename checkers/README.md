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

Notes:
====================================
* To start a new game, you have to refresh the page
* Made assumption that submission guideline meant "compiles cleanly using `lein clean; lein cljsbuild once`", not just lein cljsbuild
* Breifly looked into allowing starting a new game after a game has been won but couldn't figure it out
* Looked into memoizing the finding neighbors function, but I did not notice a unresponsiveness without it, so I did not implement it

------------------------------------------------------------------------------------------------------------

The Challenge: Checkers
====================================

The challenge is to implement the game logic for a provided checkerboard board state and UI:

* Using concurrent processes and channels in a functional manner
* Utilizing [Event Sourcing](http://www.jayway.com/2013/04/02/event-sourcing-in-clojure/) where possible.

Some notes:

* There is no 'right' answer; it is a design challenge in part, to see how the challengee think and approaches the problem.
* This is intended to take no more than a few chronological days of effort.
* The depth and scope of the attempt is up to the challenge.

Some possible functionality areas to design and implement:

* *UI event to move transformer* - take two clicks and determine if they are different board positions.  If they are, communicate a desired move to the rest of the game logic via channels.
* *A move validator* - Validate that the requested move is allowable.
* *Computer player* - this can be as stupid or as advanced as the challengee wants.
	* Major bonus points if the computer player can calculate its moves concurrently with user idle, without impact on browser responsiveness.
* *Game recorder* - record the game to a database.  One suggestion is [Datascript](https://github.com/tonsky/datascript)
	* This should be able to play back a game upon request be reproducing the event stream.

The provided code is an implementation of a rudimentary UI and board state for a checkerboard that communicates events and receives updates over channels.  It uses the following components:

* [ClojureScript](https://github.com/clojure/clojurescript) - JavaScript targeted version of Clojure, a Lisp-based language.
* [core.async](https://github.com/clojure/core.async) - Clojure library for asynchronous programming and concurrency.
* [Om](https://github.com/swannodette/om) - ClojureScript interface to FaceBook's React, that represents the UI as EDN.

Board events are sent as messages over `board-events` for backend logic to process.  The backend logic can then send the board commands via `board-commands` to update its state, although it is possible for any other process to alter the board state directly using `swap!`.

Good luck!  We have tried to come up with an interesting challenge that will help demonstrate the principles that we are trying to architect our system to.

Quickstart Notes
================

* You will need Java 7 or Java 8 JRE or JDK installed on your machine.
* Install [`lein`](http://leiningen.org/)
* Run `lein cljsbuild once`
* Open a browser on `index.html`

Our strongly suggested editor is [LightTable](http://lighttable.com/) due to the inline evaluation capabilities.  [Video](https://www.youtube.com/watch?v=cs3lO4FE3U4)

Submission Guidelines
=====================

We are providing this challenge in the form of a tarball.  We expect updated code to be returned in a tarball, compiles cleanly using `lein clean; lein cljsbuild`, and executes in a browser.
