;;; Demonstration 1 : Lisp script
;;;
;;; In this example, we create some instances of patterns,
;;; train a Chrest model with them, and then print out and 
;;; display what Chrest has learnt.
;;;
;;; Written by Peter Lane, 2010.
;;;
;;; To run this script, you will need the abcl lisp environment
;;; $ java -cp jchrest.jar:abcl.jar org.armedbear.lisp.Main --load demo-1.lisp
;;; or use
;;; $ sh run-abcl-script.sh demo-1.lisp

(load "chrest-system.lisp")
(use-package :chrest)

(let* ((model (make-chrest))
       (pattern-1 (make-list-pattern '(1 2 3)))
       (pattern-2 (make-list-pattern '(1 3 2)))
       (pattern-3 (make-list-pattern '(2 1 3)))
       (patterns (list pattern-1 pattern-2 pattern-3)))

  ;; train the model a few times on the patterns
  (dotimes (_ 4)
    (dolist (pattern patterns)
      (recognise-and-learn model pattern)))

  ;; display the results
  (format t "Current model time: ~d~&" (get-clock model))
  (dolist (pattern patterns)
    (format t "For pattern ~s model retrieves ~s~&"
            (to-string pattern)
            (to-string (recall-pattern model pattern))))

  ;; display the model in a graphical window
  (display-model model))

