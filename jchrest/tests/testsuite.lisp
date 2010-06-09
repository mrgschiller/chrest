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

;; -- global strategy tests

(def-process-tests 
  fixations ()
  (let ((fixation (make-fixation 1 4 3)))
    (assert-= (fixation-type fixation) 1)
    (assert-= (fixation-x fixation) 4)
    (assert-= (fixation-y fixation) 3)
    (assert-string= (fixation-heuristic-description fixation) "LTM heuristic")
    )
  )

;; -- run the tests
;; TODO: Catch and return gracefully from errors
(format t "Testing CHREST:~&")
(run-all-tests)

(quit)

