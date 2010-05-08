;;; This file is part of the Chrest System.
;;; Copyright (c) 2010, Peter C. R. Lane.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; ---------------------------------------------------------------------

;;; This file defines a wrapper around the java Chrest API for use within 
;;; ABCL lisp.  Using this wrapper, Chrest models can be defined and 
;;; manipulated in Lisp.

;;; Notes on using ABCL:
;;;
;;; To identify an array type, e.g. String[], use "[Ljava.lang.String;"
;;;

(defpackage :chrest
  (:use :common-lisp :java)
  (:export :make-chrest
           :get-clock
           :recognise-and-learn
           :recall-pattern
           :name-pattern
           :learn-and-name-pattern
           :display-model
           :make-list-pattern
           :make-name-pattern
           :equal-patterns-p
           :to-string
           :read-scene-data
           :create-chess-model
           :scenes-ref
           :scenes-number
           :scene-height
           :scene-width
           ))
(in-package :chrest)

(defun make-chrest ()
  "Create an instance of the Chrest class"
  (jnew (jconstructor "jchrest.architecture.Chrest")))

(defun get-clock (model)
  "Access the current clock value in model"
  (jcall (jmethod "jchrest.architecture.Chrest" "getClock")
         model))

(defun visual-ltm-size (model)
  (jcall (jmethod "jchrest.architecture.Chrest" "ltmVisualSize")
         model))

(defun visual-ltm-average-depth (model)
  (jcall (jmethod "jchrest.architecture.Chrest" "getVisualLtmAverageDepth")
         model))

(defun visual-ltm-average-image-size (model)
  (jcall (jmethod "jchrest.architecture.Chrest" "getVisualLtmAverageImageSize")
         model))

(defun recognise-and-learn (model pattern)
  "Pass the given pattern to the model to be recognised and learn"
  (jcall (jmethod "jchrest.architecture.Chrest" "recogniseAndLearn" "jchrest.lib.ListPattern")
         model
         pattern))

(defun recall-pattern (model pattern)
  "Retrieve the image pattern of the node which the given pattern sorts to"
  (jcall (jmethod "jchrest.architecture.Chrest" "recallPattern" "jchrest.lib.ListPattern")
         model
         pattern))

(defun learn-scene (model scene &optional (num-fixations 20))
  (let ((perceiver (jcall (jmethod "jchrest.architecture.Chrest" "getPerceiver")
                          model)))
    (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "setScene" "jchrest.lib.Scene")
           perceiver
           scene)
    (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "start")
           perceiver)
    (dotimes (_ num-fixations)
      (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "moveEyeAndLearn")
             perceiver))))

(defun name-pattern (model pattern)
  (jcall (jmethod "jchrest.architecture.Chrest" "namePattern" "jchrest.lib.ListPattern")
         model 
         pattern))

(defun learn-and-name-pattern (model features-pattern class-pattern)
  (jcall (jmethod "jchrest.architecture.Chrest" "learnAndNamePatterns" "jchrest.lib.ListPattern" "jchrest.lib.ListPattern")
         model
         features-pattern
         class-pattern))

(defun display-model (model)
  "Open up a graphical view of the model"
  (jnew (jconstructor "jchrest.gui.ChrestView" "jchrest.gui.Shell" "jchrest.architecture.Chrest")
        nil
        model))

(defun make-number-primitive (num)
  "Create an instance of a primitive pattern for the given number"
  (jstatic "makeNumber" "jchrest.lib.Pattern" num))

(defun make-string-primitive (str)
  (jstatic "makeString" "jchrest.lib.Pattern" str))

(defun make-java-array (item-type items)
  (let ((arr (jnew-array item-type (length items))))
    (dotimes (i (length items))
      (jarray-set arr (nth i items) i))
    arr))

(defun make-string-array (items)
  (make-java-array (jclass "java.lang.String") items))

(defun make-list-pattern (items)
  "Make a list pattern from the given items, invoking the appropriate constructor 
  for the primitive items"
  (let ((lp (jnew (jconstructor "jchrest.lib.ListPattern"))))
    (dolist (item items)
      (jcall (jmethod "jchrest.lib.ListPattern" "add" "jchrest.lib.PrimitivePattern")
             lp
             (cond ((numberp item)
                    (make-number-primitive item))
                   ((stringp item)
                    (make-string-primitive item))
                   (t
                     (error "Unknown type of item")))))
    (set-finished lp)))

(defun make-name-pattern (str)
  (set-finished
    (jstatic "makeVerbalList" 
             "jchrest.lib.Pattern" 
             (make-string-array (list str)))))

(defun set-finished (pattern)
  (jcall (jmethod "jchrest.lib.ListPattern" "setFinished")
         pattern)
  pattern)

(defun equal-patterns-p (pattern-1 pattern-2)
  (jcall (jmethod "jchrest.lib.ListPattern" "equals" "jchrest.lib.ListPattern")
         pattern-1
         pattern-2))

(defun to-string (pattern)
  "Return the string description of the given list pattern"
  (jcall (jmethod "jchrest.lib.ListPattern" "toString")
         pattern))

;; The following functions are related to running perceive-and-recall style experiments.
;; Examples of using these functions are provided for running the chess experiments.

(defun read-scene-data (filename)
  "Read in a file of scene descriptions, returning a Scenes instance"
  (let* ((file-reader (jnew (jconstructor "java.io.FileReader" "java.lang.String") filename))
         (buffered-reader (jnew (jconstructor "java.io.BufferedReader" "java.io.Reader") file-reader)))
    (handler-case
      (progn
        ;; ignore the first line, which specifies the type of data
        (jcall (jmethod "java.io.BufferedReader" "readLine") buffered-reader)
        ;; read and return the set of Scenes
        (jstatic "read" "jchrest.lib.Scenes" buffered-reader))
      (java-exception (exception)
                      (format t "Caught exception!")))))

(defun create-chess-model (scenes &optional (num-cycles 1) (num-fixations 20) (ltm-cap 1000000))
  "Create and train a new model on given set of scenes, running through the scenes the given number of times"
  (let ((model (make-chrest)))
    (jcall (jmethod "jchrest.architecture.Chrest" "setDomain" "jchrest.lib.DomainSpecifics")
           model
           (jnew (jconstructor "jchrest.lib.ChessDomain")))
    (format t "Learning~&Cycle  Visual LTM size  Avg depth  Avg image size~&")
    (dotimes (cycle num-cycles)
      (dotimes (i (scenes-number scenes))
        (learn-scene model (scenes-ref scenes i) num-fixations))
      (format t "~d ~d ~$ ~$~&"
              (1+ cycle)
              (visual-ltm-size model)
              (visual-ltm-average-depth model)
              (visual-ltm-average-image-size model)))
    model))

(defun scenes-ref (scenes i)
  "Retrieve the ith scene in scenes"
  (jcall (jmethod "jchrest.lib.Scenes" "get" "int")
         scenes
         i))

(defun scenes-number (scenes)
  (jcall (jmethod "jchrest.lib.Scenes" "size")
         scenes))

(defun scene-height (scene)
  (jcall (jmethod "jchrest.lib.Scene" "getHeight")
         scene))

(defun scene-width (scene)
  (jcall (jmethod "jchrest.lib.Scene" "getWidth")
         scene))

