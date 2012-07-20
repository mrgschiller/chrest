;;; Copyright (c) 2011-2012, Marvin Schiller,
;;; with contributions by Peter C. R. Lane.
;;; Released under Open Works License, http://owl.apotheon.org/

(load "chrest")
(use-package :chrest)

;;;;;;;;; THE FUNDAMENTALS OF THE CLASSICAL IOWA GAMBLING TASK

;;; IOWA GAMBLING TASK (SCHEDULE FROM BECHARA et al, 1994, Fig. 1)
(defparameter deckApenalties '(0 0 150 0 300 0 200 0 250  350   0 350 0  250  200 0 300 150 0 0    0    300 0 350 0  200 250 150 0   0  350 200  250 0  0   0 150 300 0  0))
(defparameter deckBpenalties '(0 0 0   0 0   0 0   0 1250 0     0 0   0  1250 0   0 0   0   0 0    1250 0   0 0   0  0   0   0   0   0  0   1250 0   0  0   0 0   0   0  0))
(defparameter deckCpenalties '(0 0 50  0 50  0 50  0 50   50    0 25  75 0    0   0 25  75  0 50   0    0   0 50  25 50  0   0   75  0  0   0    0   25 25  0 75  0   50 75))
(defparameter deckDpenalties '(0 0 0   0 0   0 0   0 0    250   0 0   0  0    0   0 0   0   0 250  0    0   0 0   0  0   0   0   250 0  0   0    0   0  250 0 0   0   0  0))

;;; Computations for the IGT - lookup of penalty (deck as 'A,'B,'C or 'D, position as list of four integers - number of cards drawn in each of the four decks)
(defun penalty (deck positions)
  (case deck 
    ('A (nth (nth 0 positions) deckApenalties))
    ('B (nth (nth 1 positions) deckBpenalties))
    ('C (nth (nth 2 positions) deckCpenalties))
    ('D (nth (nth 3 positions) deckDpenalties))))

;;; Computations for the IGT - canonical wins for the four decks
(defun win (deck)
  (case deck 
    ('A 100)
    ('B 100)
    ('C 50)
    ('D 50)))

;;; in a round-robin fashion, choose the next deck from an available set of decks
(defun get-deck-or-next-deck (deck decks)
  (if (null decks)
      NIL
      (if (find-if (lambda (x) (equal deck x)) decks)
          deck
          (cond ((equal deck 'A) (get-deck-or-next-deck 'B decks))
                ((equal deck 'B) (get-deck-or-next-deck 'C decks))
                ((equal deck 'C) (get-deck-or-next-deck 'D decks))
                ((equal deck 'D) (get-deck-or-next-deck 'A decks))))))

;;; given the current position of choices within the decks, return list of available decks
(defun available-decks (positions)
  (mapcar #'cadr (remove-if 
                  (lambda (x) (> (nth (car x) positions) 39))
                  (list (list 0 'A) (list 1 'B) (list 2 'C) (list 3 'D)))))

(defun advance-position (deck positions)
  (let ((pos1 (nth 0 positions))
        (pos2 (nth 1 positions))
        (pos3 (nth 2 positions))
        (pos4 (nth 3 positions)))
    (case deck
      ('A (incf pos1))
      ('B (incf pos2))
      ('C (incf pos3))
      ('D (incf pos4)))
    (list pos1 pos2 pos3 pos4)))

;;;;;;;;;;;; PREPARING THE CHREST MODEL -- Assumption that winning/losing money is a priority 
;;;                                        associated with joy/sadness 
      

(defun create-financial-outcome-item (model strength1 strength2)
  (let ((some-joy (chrest::clone-emotion  chrest::*pure-joy*))
        (some-sadness (chrest::clone-emotion  chrest::*pure-sadness*)))
    
    (chrest::multiply-emotion some-joy strength1)
    (chrest::multiply-emotion some-sadness strength2)
    (recognise-and-learn model (make-list-pattern (list (format NIL "OUTCOME-~A-~A" strength1 strength2))))
    (chrest::assign-emotion-to-current-item model (chrest::visual-stm model) (chrest::add-emotions some-joy some-sadness))))

(defun provide-decks (model)
  (dolist (name (list 'A 'B 'C 'D))
    (recognise-and-learn model (make-list-pattern (list (symbol-name name))))
    (chrest::assign-emotion-to-current-item model (chrest::visual-stm model) chrest::*pure-anticipation*)))

(defun provide-financial-outcomes (model)
  ;; ;; outcomes for deck A
  (create-financial-outcome-item model 100.0 0.0) ;; non-punishment
  (create-financial-outcome-item model 100.0 150.0)
  (create-financial-outcome-item model 100.0 200.0)
  (create-financial-outcome-item model 100.0 250.0)
  (create-financial-outcome-item model 100.0 300.0)
  (create-financial-outcome-item model 100.0 350.0)

  ;; outcomes for deck B
  ;; (create-financial-outcome-item model1 100.0 0.0) ;; non-punishment (same as deck A)
  (create-financial-outcome-item model 100.0 1250.0) ;; severe punishment

  ;; outcomes for deck C
  (create-financial-outcome-item model 50.0 0.0) ;; non-punishment
  (create-financial-outcome-item model 50.0 25.0) 
  (create-financial-outcome-item model 50.0 50.0) 
  (create-financial-outcome-item model 50.0 75.0) 

  ;; outcomes for deck D
  ;; (create-financial-outcome-item model1 50.0 0.0) ;; non-punishment (same as deck C)
  (create-financial-outcome-item model 50.0 250.0) 
  )

;;;;;;;;; service functions for "learning" and "recognising" decks

(defun experience-deck (model deckname positions)
  (let* ((win (* 1.0 (win deckname)))
         (penalty (* 1.0 (penalty deckname positions)))
         (outcome-pattern (make-list-pattern (list (format NIL "OUTCOME-~A-~A" win penalty)))))
    (format t "Experience deck ~A, win: ~A, lose: ~A~%" deckname win penalty)
    (chrest::stm-clear (chrest::visual-stm model)) ;;; this is artificial
    (recognise-and-learn model (make-list-pattern (list (symbol-name deckname))))
    (chrest::recall-pattern model outcome-pattern)
    (chrest::emote-and-propagate-across-modalities model (list (chrest::visual-stm model)))))

(defun collect-deck-emotion (model deckname)
  (chrest::stm-clear (chrest::visual-stm model))
  (recognise-and-learn model (make-list-pattern (list (symbol-name deckname))))
  (prog1 
    (chrest::get-current-emotion model (chrest::visual-stm model))
    (chrest::stm-clear (chrest::visual-stm model))))

;; helper function

(defun get-maxima (lst)
  (let ((max (reduce (lambda (x y) (if (> x y) x y)) (mapcar #'cadr lst))))
    (mapcar #'car (remove-if (lambda (x) (< (cadr x) max)) lst))))


;;; iterate card selections within one model

(defun iterate-gambling-task (model runs-left position balance selections randomness emotionevalfun ranselect previousselection anticipfactor)
  (if (not (zerop runs-left))
        (let* ((void (setf *random-state* (make-random-state t)))
               (remaining-decks (available-decks position))
               (deck-values (mapcar (lambda (x) (funcall emotionevalfun model x anticipfactor)) remaining-decks))
               (deck (car (get-maxima (mapcar #'list remaining-decks deck-values))))
               (deck (if (<= (+ (random 100) 1) randomness) ;; introduce random round robin skips
                         (progn
                           (setq ranselect (+ ranselect 1))
                           (get-deck-or-next-deck previousselection (remove-if (lambda (x) (equal x previousselection)) remaining-decks)))
                         deck))
               (newposition (advance-position deck position)))
          (format t "Position: ~A ~%" newposition)
          (experience-deck model deck position)
          (iterate-gambling-task model 
                                 (1- runs-left) 
                                 newposition 
                                 (+ balance (- (win deck) (penalty deck position))) 
                                 (cons deck selections) 
                                 randomness 
                                 emotionevalfun 
                                 ranselect 
                                 deck 
                                 anticipfactor))
      ;;; done iterating
      (progn 
        (format t "Final balance: ~A~%" balance)
        selections)))


;;; RUN OF THE MODEL (until schedule is exhausted)


(defun run-model (counts randomness emotionevalfun alpha anticipfactor)
  (if (equal counts 0)
      NIL
      (cons (run-model-once randomness emotionevalfun alpha anticipfactor) (run-model (- counts 1) randomness emotionevalfun alpha anticipfactor))))

(defun run-model-once (randomness emotionevalfun alpha anticipfactor)
  (let ((model (make-chrest))
        (runs 100)
        (position (list 0 0 0 0))
        (balance 0)
        (selections NIL))
    (setq model1 model)
    (chrest::set-default-alpha model alpha)
    (provide-decks model)
    (provide-financial-outcomes model)
    (setq selections (iterate-gambling-task model runs position balance selections randomness emotionevalfun 0 'D anticipfactor))  ;; roundrobin=-1
    ; (chrest::display-model2 model)
    ; (setq *distributions* (cons selections *distributions*))
    (format t "Distribution of selections: ~A" (mapcar (lambda (y) (length (remove-if (lambda (x) (not (equal x y))) selections))) (list 'A 'B 'C 'D)))
    (reverse selections)))


(defun evaluate-deck (model deck anticipfactor)
  (chrest::stm-clear (chrest::visual-stm model))
  (recognise-and-learn model (make-list-pattern (list (symbol-name deck))))
  (let ((emotion (chrest::get-current-emotion model (chrest::visual-stm model))))
    (if (null emotion) -1000
      (let* ((joy (chrest::emotion-project emotion chrest::*pure-joy*))
             (sadness (chrest::emotion-project emotion chrest::*pure-sadness*))
             (anticipation (chrest::emotion-project emotion chrest::*pure-anticipation*)))
        (chrest::stm-clear (chrest::visual-stm model))
        (+ (* anticipfactor anticipation) (* (- 1 anticipfactor) (- joy sadness)))))))


;;;;; One run of the model with the given parameters

(let ((randomness 30)    ;;; percentage of randomly skipping to the next deck (0-100)
      (alpha 0.2)        ;;; learning rate (0-1.0)
      (anticipation 0.5) ;;; relative importance of discovering decks (vs. using available knowledge), range 0 (use only learned knowledge) - 1.0 (learned knowledge is ignored)
      )
  (run-model-once randomness #'evaluate-deck alpha anticipation))

(exit)
