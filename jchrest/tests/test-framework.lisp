;;; This file is part of the Chrest System.
;;; Copyright (c) 2010, Peter C. R. Lane.
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; 
;;; ---------------------------------------------------------------------

;;; ABOUT THIS SOFTWARE
;;;
;;; The functions in this file support use of a testing framework for 
;;; scientific theories.  Tests are separated into three classes, as 
;;; described in [1]:
;;; 1. Unit tests - for implementational details
;;; 2. Process tests - for the main processes within the theory
;;; 3. Canonical results - for the experimental results achieved by the
;;;                        theory
;;; [1] P.C.R. Lane and F. Gobet, Developing reproducible and comprehensible
;;;     computational models, Artificial Intelligence, 144:251--63, 2003.

;;; INSTRUCTIONS FOR USE 
;;; 
;;; Individual tests are written in one of the following forms:
;;; (test THING-TO-CHECK [MESSAGE])
;;;       where: THING-TO-CHECK is a test, returning a boolean value
;;;                             test should return T if the test passed 
;;;              MESSAGE is optional, and is given if the test is NIL
;;; (assert-= EXPECTED ACTUAL [MESSAGE])
;;; (assert-eq EXPECTED ACTUAL [MESSAGE])
;;; (assert-equalp EXPECTED ACTUAL [MESSAGE])
;;; (assert-string= EXPECTED ACTUAL [MESSAGE])
;;;       where: EXPECTED is the predicted value
;;;              ACTUAL is the computed value
;;;              EXPECTED and ACTUAL are compared using respective function
;;;              MESSAGE is optional, and given if the comparison fails
;;; (assert-true THING-TO-CHECK [MESSAGE]) is the same as (test THING-TO-CHECK MESSAGE)
;;; (assert-false THING-TO-CHECK [MESSAGE]) is the same as (test (not THING-TO-CHECK) MESSAGE)
;;; (assert-null THING-TO-CHECK [MESSAGE]) is the same as (test (null THING-TO-CHECK) MESSAGE)
;;;
;;; To create a group of tests, use either of: 
;;; def-unit-tests
;;; def-process-tests
;;; def-canonical-result-tests
;;; All three do the same job, which is to define a function containing one or 
;;; more calls to (test .. ..)
;;; The framework stores the functions in one of the three categories of tests.
;;;
;;; Tests can be run using either of:
;;; (run-unit-tests)             evaluates every function defined using def-unit-tests
;;; (run-process-tests)          evaluates every function defined using def-process-tests
;;; (run-canonical-result-tests) evaluates every function defined using def-canonical-result-tests
;;; (run-all-tests)              evaluates all three of the above, running all tests

(let ((error-count 0)
      (total-tests 0)
      (unit-tests ())
      (process-tests ())
      (canonical-results-tests ()))
  (defun reset-error-count ()
	 (setf error-count 0
	       total-tests 0))
  (defun error-feedback ()
	 (format t "~%=== DONE: There ~a ~a error~a in ~a test~a~%" 
		 (if (= 1 error-count) "was" "were")
		 error-count 
		 (if (= 1 error-count) "" "s")
		 total-tests
		 (if (= 1 total-tests) "" "s")))

  ;; the basic test function
  (defun test (bool &optional (msg ""))
	 "If bool is true, display a dot, else the message"
	 (incf total-tests)
	 (if bool
	     (format t ".")
	     (format t "~&Error ~a: ~a~&" (incf error-count) msg)))
  
  ;; some customised test functions, to provide a more direct test syntax
  (flet ((test-compare (comp-fn expected actual &optional (msg ""))
		       (test (funcall comp-fn expected actual)
			     (format nil "Expected ~a got ~a. ~a" expected actual msg))))
	(defun assert-eq (expected actual &optional (msg ""))
		(test-compare #'eq expected actual msg))
	(defun assert-equalp (expected actual &optional (msg ""))
	       (test-compare #'equalp expected actual msg))
	(defun assert-= (expected actual &optional (msg ""))
	       (test-compare #'= expected actual msg))
	(defun assert-string= (expected actual &optional (msg ""))
	       (test-compare #'string= expected actual msg)))
  (defun assert-true (bool &optional (msg ""))
	 (test bool msg))
  (defun assert-false (bool &optional (msg ""))
	 (test (not bool) msg))
  (defun assert-null (item &optional (msg ""))
	 (test (null item) msg))
  
  ;; macros for creating functions containing tests, placing the tests into groups
  (defmacro def-unit-tests (name &rest body)
	    (push name unit-tests)
	    `(defun ,name ,@body))
  (defmacro def-process-tests (name &rest body)
	    (push name process-tests)
	    `(defun ,name ,@body))
  (defmacro def-canonical-result-tests (name &rest body)
	    (push name canonical-results-tests)
	    `(defun ,name ,@body))

  ;; functions for running the tests
  (flet ((run-tests (name test-list)
		    (format t "Running ~a: " name)
		    (reset-error-count)
		    (dolist (test test-list) (funcall test))
		    (error-feedback)))
	(defun run-unit-tests () (run-tests "Unit tests" unit-tests))
	(defun run-process-tests () (run-tests "Process tests" process-tests))
	(defun run-canonical-result-tests () 
	       (run-tests "Canonical results" canonical-results-tests)))
  (defun run-all-tests ()
	 (run-unit-tests)
	 (run-process-tests)
	 (run-canonical-result-tests)))

#|
;;; example of use

(defun adder (x y) (+ x y))
(defun multiplier (x y) (* x y))

(def-unit-tests x ()
  (test (= 4 (adder 2 2)) "test 1")
  (test (= 5 (adder 2 3)) "test 2")
  (assert-equalp 4 (adder 2 2)))

(def-unit-tests x2 ()
  (assert-= 7 (adder 2 1))
  (assert-= 7 (adder 2 1) "adding error"))

(def-unit-tests x3 ()
  (assert-true (< 2 (adder 2 1)))
  (assert-false (> 2 (adder 2 1))))

(def-process-tests y ()
  (test (= 4 (multiplier 2 2))))

(def-canonical-result-tests z ()
  (test (= 20 (multiplier (adder 2 3) 4))))

(run-all-tests) ;; runs all the defined tests, separated into groups

;; you can run an individual test function by calling it:
;; (x)
;; or the functions from one group by calling the relevant 'run' function
;; (run-unit-tests)
|#

