(load "test-framework")
(load "../scripts/chrest-system.lisp")
(use-package :chrest)
;; -- pattern tests

;; -- chess domain tests
(def-unit-tests 
  chess-domain ()
  (let ((ios-1 (make-item-on-square "P" 1 1))
        (ios-2 (make-item-on-square "Q" 2 2))
        (ios-3 (make-item-on-square "q" 2 2))
        (ios-4 (make-item-on-square "Q" 2 7))
        (ios-5 (make-item-on-square "q" 2 7)))
    (assert-false (big-piece-p ios-1))
    (assert-true (big-piece-p ios-2))
    (assert-false (offensive-piece-p ios-2))
    (assert-true (offensive-piece-p ios-3))
    (assert-true (offensive-piece-p ios-4))
    (assert-false (offensive-piece-p ios-5))
    (let* ((lp (make-list-pattern (list ios-1 ios-2 ios-3 ios-4 ios-5)))
           (novice (get-salient-pieces lp nil))
           (expert (get-salient-pieces lp t)))
      (assert-= (pattern-size novice) 4)
      (assert-= (pattern-size expert) 2))
    ))

(def-unit-tests
  chess-moves ()
  (let ((board-1 (construct-chess-board "......../......../......../....N.../......../......../......../........"))
        (board-2 (construct-chess-board "......../......../......../....N.../......p./......../......../........"))
        (board-3 (construct-chess-board "......../......../......../....N.../......P./......../......../........"))
        (board-4 (construct-chess-board "N......./......../......../......../......../......../......../........"))
        (board-5 (construct-chess-board "N...R.p./....P.../R...k.../.......R/......../......../......../........"))
        )
    (assert-false (scene-empty-square-p board-1 3 4))
    (assert-= 8
              (length (find-moves board-1 (make-square 3 4)))
              "knight moves on open board")
    (assert-= 8
              (length (find-moves board-2 (make-square 3 4)))
              "knight moves on open board - can capture")
    (assert-= 7
              (length (find-moves board-3 (make-square 3 4)))
              "knight moves on open board - obstructed")
    (assert-= 2
              (length (find-moves board-4 (make-square 0 0)))
              "knight moves from corner")
    (assert-= 5
              (length (find-moves board-5 (make-square 0 4)))
              "rook moves from (e8)")
    (assert-= 10
              (length (find-moves board-5 (make-square 2 0)))
              "rook moves from (a6)")
    (assert-= 14
              (length (find-moves board-5 (make-square 3 7)))
              "rook moves from (h5)")
    ))

;; -- global strategy tests

;; -- run the tests
;; TODO: Catch and return gracefully from errors
(format t "Testing CHREST:~&")
(run-all-tests)

(quit)

