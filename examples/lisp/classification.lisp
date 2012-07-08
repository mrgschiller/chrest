;;; classification.lisp

;;; Written by Peter Lane, 2010.

;;; Simple illustration of using Chrest to perform classification

(load "chrest-system")
(use-package :chrest)

(defstruct example features class)

(defun construct-patterns (days)
  "Construct a list pattern given a list of day definitions"
  (mapcar #'(lambda (day)
              (make-example 
               :features (make-list-pattern 
                          (list (concatenate 'string "outlook-" (nth 0 day))
                                (concatenate 'string "temperature-" (nth 1 day))
                                (concatenate 'string "humidity-" (nth 2 day))
                                (concatenate 'string "windy-" (nth 3 day))))
               :class (make-name-pattern (nth 4 day))))
          days))

(defconstant *weather* 
             (construct-patterns 
               '(("sunny" "hot" "high" "false" "no")
                 ("sunny" "hot" "high" "true" "no")
                 ("overcast" "hot" "high" "false" "yes")
                 ("rainy" "mild" "high" "false" "yes")
                 ("rainy" "cool" "normal" "false" "yes")
                 ("rainy" "cool" "normal" "true" "no")
                 ("overcast" "cool" "normal" "true" "yes")
                 ("sunny" "mild" "high" "false" "no")
                 ("sunny" "cool" "normal" "false" "yes")
                 ("rainy" "mild" "normal" "false" "yes")
                 ("sunny" "mild" "normal" "true" "yes")
                 ("overcast" "mild" "high" "true" "yes")
                 ("overcast" "hot" "normal" "false" "yes")
                 ("rainy" "mild" "high" "true" "no"))))

(let ((model (make-chrest)))
  (dotimes (i 15)
    (dolist (instance *weather*)
      (learn-and-name-pattern model (example-features instance) (example-class instance)))
    (format t "Performance on cycle ~d is: " (1+ i))
    (let ((sum 0))
      (dolist (instance *weather*)
	(unless (null (name-pattern model (example-features instance)))
	  (when (equal-patterns-p (example-class instance) 
				  (name-pattern model (example-features instance)))
	    (incf sum))))
      (format t "~$~&" (/ sum (length *weather*)))))

  (display-model model))

