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

(defpackage :chrest
  (:use :common-lisp :java)
  (:export :make-chrest
           :get-clock
           :get-ltm-size
           :recognise-and-learn
           :recall-pattern
           :name-pattern
           :learn-and-name-pattern
           :recall-scene
           :results-recall
           :results-proportion-fixated-squares
           :results-proportion-fixated-pieces
           :display-model
           :make-list-pattern
           :make-name-pattern
           :equal-patterns-p
           :to-string
           :read-scene-data
           :construct-chess-board
           :create-chess-model
           :scenes-ref
           :scenes-number
           :scene-height
           :scene-width
           :average
           ))
(in-package :chrest)

(defun make-chrest ()
  "Create an instance of the Chrest class"
  (jnew (jconstructor "jchrest.architecture.Chrest")))

(defun get-clock (model)
  "Access the current clock value in model"
  (jcall (jmethod "jchrest.architecture.Chrest" "getClock")
         model))

(defun get-ltm-size (model)
  (jcall (jmethod "jchrest.architecture.Chrest" "getTotalLtmNodes")
         model))

(defun visual-ltm-average-depth (model)
  (jcall (jmethod "jchrest.architecture.Chrest" "getVisualLtmAverageDepth")
         model))

(defun visual-ltm-average-image-size (model)
  (jcall (jmethod "jchrest.architecture.Chrest" "getVisualLtmAverageImageSize")
         model))

(defun visual-stm (model)
  (jcall (jmethod "jchrest.architecture.Chrest" "getVisualStm")
         model))

(defun get-stm-item (stm index)
  (jcall (jmethod "jchrest.architecture.Stm" "getItem" "int")
         stm
         index))

(defun visual-stm-count (stm)
  (jcall (jmethod "jchrest.architecture.Stm" "getCount")
         stm))

(defun get-image (node)
  (jcall (jmethod "jchrest.architecture.Node" "getImage")
         node))

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

(defun get-perceiver (model)
  (jcall (jmethod "jchrest.architecture.Chrest" "getPerceiver")
         model))

(defun get-number-fixations (perceiver)
  (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "getNumberFixations")
         perceiver))

(defun get-fixation-x (perceiver index)
  (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "getFixationsX" "int")
         perceiver
         index))

(defun get-fixation-y (perceiver index)
  (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "getFixationsY" "int")
         perceiver
         index))

(defun scan-scene (model scene &optional (num-fixations 20))
  (let ((perceiver (get-perceiver model)))
    (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "setScene" "jchrest.lib.Scene")
           perceiver
           scene)
    (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "start")
           perceiver)
    (dotimes (_ num-fixations)
      (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "moveEye")
             perceiver))))

(defun item-square-p (pattern)
  (handler-case 
    (progn (jcall (jmethod "jchrest.lib.ItemSquarePattern" "getItem")
                  pattern)
           t)
    (java-exception (exception)
                    nil)))

(defstruct fixation row col)

(defun get-fixation-list (model)
  (let ((fixation-list ()))
    (dotimes (i (get-number-fixations (get-perceiver model)))
      (push (make-fixation :row (get-fixation-x (get-perceiver model) i)
                           :col (get-fixation-y (get-perceiver model) i))
            fixation-list))
    (reverse fixation-list)))

(defun fixated-squares (model)
  (remove-duplicates (get-fixation-list model)
                     :test #'(lambda (fixation-1 fixation-2)
                               (and (= (fixation-row fixation-1) 
                                       (fixation-row fixation-2))
                                    (= (fixation-col fixation-1)
                                       (fixation-col fixation-2))))))

(defun proportion-squares-fixated (model)
  (/ (length (fixated-squares model)) 64))

(defun proportion-pieces-fixated (model scene)
  (/ (count-if-not #'(lambda (fixation) 
                       (scene-empty-square-p scene 
                                             (fixation-row fixation)
                                             (fixation-col fixation)))
                   (fixated-squares model))
     (count-scene-items scene)))

(defstruct results
  num-target-items
  num-recalled-items
  num-correct
  recall
  precision
  proportion-fixated-squares
  proportion-fixated-pieces)

(defun recall-scene (model scene &optional (num-fixations 20))
  "Scan the given scene, then return an instance of 'results' structure 
  containing relevant information."
  (scan-scene model scene num-fixations)
  (let ((recalled-items ()))
    (dotimes (i (visual-stm-count (visual-stm model)))
      (let ((image (get-image (get-stm-item (visual-stm model) i))))
        (dotimes (j (pattern-size image))
          (when (item-square-p (get-pattern-item image j))
            (push (get-pattern-item image j) recalled-items)))))
    (setf recalled-items
          (remove-duplicates recalled-items
                       :test
                       #'(lambda (pattern-1 pattern-2)
                           (jcall (jmethod "jchrest.lib.PrimitivePattern" "equalPrimitive" "jchrest.lib.PrimitivePattern")
                                  pattern-1
                                  pattern-2))))
    (make-results
      :num-target-items (count-scene-items scene)
      :num-recalled-items (length recalled-items)
      :num-correct (count-scene-overlap scene recalled-items)
      :recall (compute-scene-recall scene recalled-items)
      :precision 0
      :proportion-fixated-squares (proportion-squares-fixated model)
      :proportion-fixated-pieces (proportion-pieces-fixated model scene))))

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

(defun pattern-size (pattern)
  (jcall (jmethod "jchrest.lib.ListPattern" "size")
         pattern))
  
(defun get-pattern-item (pattern index)
  (jcall (jmethod "jchrest.lib.ListPattern" "getItem" "int")
         pattern
         index))

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

(defun construct-chess-board (defn &optional (name "chess board"))
  "Construct a chess board from a string representation"
  (let ((scene (jnew (jconstructor "jchrest.lib.Scene" "java.lang.String" "int" "int")
                     name
                     8
                     8)))
    (dotimes (row 8)
      (dotimes (col 8)
        (let ((item (string (aref defn (+ (* 9 row) col)))))
          (jcall (jmethod "jchrest.lib.Scene" "setItem" "int" "int" "java.lang.String")
                 scene
                 row
                 col
                 item))))
    scene))

(defun create-chess-model (scenes &optional (num-cycles 1) (num-fixations 20) (ltm-cap 1000000))
  "Create and train a new model on given set of scenes, running through the scenes the 
  given number of times.  Training ceases when the model's LTM size reaches the ltm-cap."
  (let ((model (make-chrest)))
    (jcall (jmethod "jchrest.architecture.Chrest" "setDomain" "jchrest.lib.DomainSpecifics")
           model
           (jnew (jconstructor "jchrest.lib.ChessDomain")))
    (format t "Learning~&Cycle  Visual LTM size  Avg depth  Avg image size~&")
    (do ((cycle 0 (1+ cycle)))
      ((or (> (get-ltm-size model) ltm-cap)
           (= cycle num-cycles))
       model)
      (do ((i 0 (1+ i)))
        ((or (> (get-ltm-size model) ltm-cap)
             (= i (scenes-number scenes)))
         (format t "~5d  ~15d  ~9,2F  ~14,2F~&"
                 (1+ cycle)
                 (get-ltm-size model)
                 (visual-ltm-average-depth model)
                 (visual-ltm-average-image-size model)))
        (learn-scene model (scenes-ref scenes i) num-fixations)))))

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

(defun scene-empty-square-p (scene row col)
  (jcall (jmethod "jchrest.lib.Scene" "isEmpty" "int" "int")
         scene
         row
         col))

(defun count-scene-items (scene)
  (let ((result 0))
    (dotimes (row 8)
      (dotimes (col 8)
        (unless (scene-empty-square-p scene row col)
          (incf result))))
    result))

(defun compute-scene-recall (scene items)
  (/ (count-scene-overlap scene items)
     (count-scene-items scene)))

(defun count-scene-overlap (scene items)
  "Return a count of the number of items in given list which are correctly located in scene"
  (let ((result 0))
    (dolist (item items)
      (when (string= (jcall (jmethod "jchrest.lib.ItemSquarePattern" "getItem")
                            item)
                     (jcall (jmethod "jchrest.lib.Scene" "getItem" "int" "int")
                            scene
                            (1-
                             (jcall (jmethod "jchrest.lib.ItemSquarePattern" "getRow")
                                    item))
                            (1-
                             (jcall (jmethod "jchrest.lib.ItemSquarePattern" "getColumn")
                                    item))))
        (incf result)))
    result))

(defun average (results)
  (* 100 (/ (apply #'+ results)
            (length results))))
