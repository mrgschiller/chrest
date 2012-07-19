;;; Copyright (c) 2010-12, Peter C. R. Lane,
;;; with contributions by Marvin Schiller.
;;; Released under Open Works License, http://owl.apotheon.org/

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
           :pattern-size
           :to-string
           :read-scene-data
           :construct-chess-board
           :create-chess-model
           :scenes-ref
           :scenes-number
           :scene-height
           :scene-width
           :average
           ; for chess domain
           :scene-empty-square-p
           :make-square
           :find-moves
           ; for fixations
           :make-fixation
           :fixation-type
           :fixation-x
           :fixation-y
           :fixation-heuristic-description
           :make-item-on-square
           :big-piece-p
           :offensive-piece-p
           :get-salient-pieces
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
  (jcall (jmethod "jchrest.architecture.Chrest" "learnScene" "jchrest.lib.Scene" "int")
         model
         scene
         num-fixations))

(defun get-perceiver (model)
  (jcall (jmethod "jchrest.architecture.Chrest" "getPerceiver")
         model))

(defun get-number-fixations (perceiver)
  (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "getNumberFixations")
         perceiver))

(defun get-fixation-type (perceiver index)
  (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "getFixationsType" "int")
         perceiver
         index))

(defun get-fixation-x (perceiver index)
  (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "getFixationsX" "int")
         perceiver
         index))

(defun get-fixation-y (perceiver index)
  (jcall (jmethod "jchrest.architecture.Chrest$Perceiver" "getFixationsY" "int")
         perceiver
         index))

(defun scan-scene (model scene &optional (num-fixations 20))
  "Returns a Scene, as would be recalled using information in STM"
  (jcall (jmethod "jchrest.architecture.Chrest" "scanScene" "jchrest.lib.Scene" "int")
         model
         scene 
         num-fixations))

(defun make-item-on-square (item row col)
  (jnew (jconstructor "jchrest.lib.ItemSquarePattern" "java.lang.String" "int" "int")
        item
        row
        col))

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
      (push (make-fixation 
              :row (get-fixation-x (get-perceiver model) i)
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
  proportion-fixated-squares
  proportion-fixated-pieces)

(defun recall-scene (model scene &optional (num-fixations 20))
  "Scan the given scene, then return an instance of 'results' structure 
  containing relevant information."
  (let ((recalled-scene (scan-scene model scene num-fixations)))
    (make-results
      :num-target-items (count-scene-items scene)
      :num-recalled-items (count-scene-items recalled-scene)
      :num-correct (count-scene-overlap scene recalled-scene)
      :recall (compute-scene-recall scene recalled-scene)
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
  (jnew (jconstructor "jchrest.gui.ChrestView" "jchrest.architecture.Chrest")
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
  for the primitive items where needed"
  (let ((lp (jnew (jconstructor "jchrest.lib.ListPattern"))))
    (dolist (item items)
      (jcall (jmethod "jchrest.lib.ListPattern" "add" "jchrest.lib.PrimitivePattern")
             lp
             (cond ((numberp item)
                    (make-number-primitive item))
                   ((stringp item)
                    (make-string-primitive item))
                   (t
                     item))))
    (set-finished lp)
    lp))

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

(defun count-scene-overlap (scene-1 scene-2)
  (jcall (jmethod "jchrest.lib.Scene" "countOverlappingPieces" "jchrest.lib.Scene")
         scene-1 
         scene-2))

(defun count-scene-items (scene)
  (jcall (jmethod "jchrest.lib.Scene" "countItems")
         scene))

(defun compute-scene-recall (scene recalled)
  (jcall (jmethod "jchrest.lib.Scene" "computeRecall" "jchrest.lib.Scene")
         scene
         recalled))

(defun average (results)
  (* 100 (/ (apply #'+ results)
            (length results))))

(defun make-square (x y)
  (jnew (jconstructor "jchrest.lib.Square" "int" "int")
        x 
        y))

;; need to convert java list into a lisp list
(defun find-moves (board square)
  (let ((result ())
        (moves 
          (jcall (jmethod "jchrest.lib.ChessDomain" "proposeMovementFixations" "jchrest.lib.Scene" "jchrest.lib.Square")
                 (jnew (jconstructor "jchrest.lib.ChessDomain"))
                 board
                 square)))
    (dotimes (i (jcall (jmethod "java.util.List" "size") moves))
      (push (jcall (jmethod "java.util.List" "get" "int") moves i)
            result))
    (reverse result)))

;(defun make-fixation (fixation-type x y)
;  (jnew (jconstructor "jchrest.lib.Fixation" "int" "int" "int")
;        fixation-type
;        x
;        y))

(defun fixation-type (fixation)
  (jcall (jmethod "jchrest.lib.Fixation" "getType")
         fixation))

(defun fixation-x (fixation)
  (jcall (jmethod "jchrest.lib.Fixation" "getX")
         fixation))

(defun fixation-y (fixation)
  (jcall (jmethod "jchrest.lib.Fixation" "getY")
         fixation))

(defun fixation-heuristic-description (fixation)
  (jcall (jmethod "jchrest.lib.Fixation" "getHeuristicDescription")
         fixation))

;; for chess domain, calling static methods
(defun big-piece-p (ios) 
  (jstatic "isBigPiece" "jchrest.lib.ChessDomain" ios))

(defun offensive-piece-p (ios)
  (jstatic "isOffensivePiece" "jchrest.lib.ChessDomain" ios))

(defun get-salient-pieces (list-pattern experiencedp)
  (jstatic "getSalientPieces" 
           "jchrest.lib.ChessDomain" 
           list-pattern
           (if experiencedp 1 0)))

;;;;;;;;; The following is for using the emotion association mechanism (M. Schiller)

;;; retrieve pointers to all available theories
(defun chrest::get-all-emotion-theories ()
  (coerce (jstatic "getAllEmotionTheories" "jchrest.architecture.Emotion") 'list)
)

(defparameter *plutchik* (car (get-all-emotion-theories)))
(defparameter *ekman*   (cadr (get-all-emotion-theories)))

;;; retrieve pointers to all available basic ("primitive") emotions for a theory
(defun chrest::list-pure-emotions (theory strength)
  (coerce (jstatic "listPureEmotions" "jchrest.architecture.Emotion" theory strength) 'list)
)

(defparameter *plutchik-emotions* (list-pure-emotions *plutchik* 1.0))
(defparameter *pure-joy*          (nth 0 *plutchik-emotions*))
(defparameter *pure-trust*        (nth 1 *plutchik-emotions*))
(defparameter *pure-fear*         (nth 2 *plutchik-emotions*))
(defparameter *pure-surprise*     (nth 3 *plutchik-emotions*))
(defparameter *pure-sadness*      (nth 4 *plutchik-emotions*))
(defparameter *pure-disgust*      (nth 5 *plutchik-emotions*))
(defparameter *pure-anger*        (nth 6 *plutchik-emotions*))
(defparameter *pure-anticipation* (nth 7 *plutchik-emotions*))


(defparameter *ekman-emotions* (list-pure-emotions *ekman* 1.0))
(defparameter *pure-ekman-amusement*           (nth 0 *ekman-emotions*))
(defparameter *pure-ekman-anger*               (nth 1 *ekman-emotions*))
(defparameter *pure-ekman-contempt*            (nth 2 *ekman-emotions*))
(defparameter *pure-ekman-contentment*         (nth 3 *ekman-emotions*))
(defparameter *pure-ekman-disgust*             (nth 4 *ekman-emotions*))
(defparameter *pure-ekman-embarassment*        (nth 5 *ekman-emotions*))
(defparameter *pure-ekman-excitement*          (nth 6 *ekman-emotions*))
(defparameter *pure-ekman-fear*                (nth 7 *ekman-emotions*))
(defparameter *pure-ekman-guilt*               (nth 8 *ekman-emotions*))
(defparameter *pure-ekman-prideinachievement*  (nth 9 *ekman-emotions*))
(defparameter *pure-ekman-relief*              (nth 10 *ekman-emotions*))
(defparameter *pure-ekman-sadnessdistress*     (nth 11 *ekman-emotions*))
(defparameter *pure-ekman-satisfaction*        (nth 12 *ekman-emotions*))
(defparameter *pure-ekman-sensorypleasure*     (nth 13 *ekman-emotions*))
(defparameter *pure-ekman-shame*               (nth 14 *ekman-emotions*))

;;; Given a complex emotion and a basic emotion, retrieve the strength of the complex emotion on the dimension of the basic emotion 
(defun chrest::emotion-project (emotion pureemotion)
  (jcall (jmethod "jchrest.architecture.Emotion" "project" "jchrest.architecture.Emotion")
          emotion
          pureemotion
          )
  )

(defun chrest::set-default-alpha (model alpha)
  (jcall (jmethod "jchrest.architecture.Chrest" "setDefaultAlpha" "double")
         model 
         alpha
         ))

(defun chrest::stm-clear(stm)
  (jcall (jmethod "jchrest.architecture.Stm" "clear")
         stm))

(defun chrest::print-emotion (emotion theory)
  ; (format t "EMOTION (strength ~A)~%" (get-emotion-strength emotion))
  (mapcar (lambda (x y) 
            (format t "~A  ~A ~%" y (emotion-project emotion x))) *plutchik-emotions* 
          (if (equal theory *plutchik*) (list "joy" "trust" "fear" "surprise" "sadness" "disgust" "anger" "anticipation")
              (list "amusement" "anger" "contempt" "contentment" "disgust" "embarassement" 
                    "excitement" "fear" "guilt" "prideinachievement" "relief" "sadnessdistress" "satisfaction" "sensorypleasure" "shame"))
              )
  )

;;; mainpulating emotions
(defun chrest::clone-emotion (emotion)
  (jstatic "clone" "jchrest.architecture.Emotion" emotion))
        
(defun chrest::add-emotions (emotion1 emotion2)
  (jstatic "add" "jchrest.architecture.Emotion" emotion1 emotion2))

        
(defun chrest::multiply-emotion (emotion factor)
  (jcall (jmethod "jchrest.architecture.Emotion" "multiply" "float")
         emotion
         factor
         ))

;;; use the emotion association mechanism, where the most recent item in the STMs is considered the unconditioned stimulus, and the others as conditioned stimuli.
(defun chrest::emote-and-propagate-across-modalities (model stms)
  (jcall (jmethod "jchrest.architecture.Chrest" "emoteAndPropagateAcrossModalities" "java.lang.Object")
         model 
         (chrest::make-java-array (jclass "jchrest.architecture.Stm") stms)))

;;; highhandedly assign an emotion to the most recent item in STM         
(defun chrest::assign-emotion-to-current-item (model stm emotion)
  (jcall (jmethod "jchrest.architecture.Chrest" "assignEmotionToCurrentItem" "jchrest.architecture.Stm" "jchrest.architecture.Emotion")
         model 
         stm
         emotion
         ))

;;; retrieve the emotion associated with the most recent item in STM
(defun chrest::get-current-emotion (model stm)
  (jcall (jmethod "jchrest.architecture.Chrest" "getCurrentEmotion" "jchrest.architecture.Stm")
         model 
         stm
         ))
