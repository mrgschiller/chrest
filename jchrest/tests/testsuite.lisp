(load "test-framework")
(load "../scripts/chrest-system.lisp")

;; -- pattern tests

;; -- global strategy tests

(def-process-tests fixations ()
  (let ((fixation (chrest:make-fixation 1 4 3)))
    (assert= (chrest:fixation-type fixation) 1)
    (assert= (chrest:fixation-x fixation) 4)
    (assert= (chrest:fixation-y fixation) 3)
    (assert-string= (chrest:fixation-heuristic-description fixation) "LTM heuristic")
    )
  )

;; -- run the tests
;; TODO: Catch and return gracefully from errors
(format t "Testing CHREST:~&")
(run-all-tests)

(quit)

