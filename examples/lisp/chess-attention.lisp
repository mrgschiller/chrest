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

;;; Chess: de Groot and Gobet (1996)
;;; --------------------------------
;;; 
;;; The book by de Groot and Gobet (1996) contains detailed analysis of chess
;;; players' eye movements and protocol analysis.  Also included is a set of
;;; experiments using an earlier version of CHREST providing evidence of the speed
;;; and quality of eye fixations of masters over novices, and also the qualitative
;;; difference in recall performance of masters over novices.  
;;; 
;;; The precise training data from the book could not be precisely reproduced, but
;;; the file 'chess-positions.dat' contains 8644 positions, including a
;;; range of middle game positions and opening positions, which are a close 
;;; approximation to the 9500 positions described in the book.  
;;; 
;;; The test data were six positions from Chapter 6 of the book; these positions 
;;; do not occur in the training set.  These positions are included within the 
;;; code below, along with each position's square-relevance, from Figure 6.8 of 
;;; the book.
;;; 
;;; To emulate the book results, the Novice model is trained to have 200 chunks 
;;; within its long-term memory, and the Master model to have 24,000 chunks.
;;; 
;;; Testing uses six test positions, looking at four sets of data:
;;; 
;;; 1. the mean and variance of the duration of eye fixations.
;;; 2. the percentage of the board covered.
;;; 3. the relevance of the squares covered. 
;;; 4. the recall performance on the test positions.
;;; 
;;; Note that the version of CHREST in the book included the complete set of
;;; attention heuristics and STM mechanisms.  Also, all learning mechanisms for
;;; building a discrimination network.  However, templates were not used for the
;;; results in the book, leading to relatively low scores in the results for recall
;;; performance, when compared to real master players.
;;; 
;;; Running the following program will construct CHREST models as described 
;;; above, test the models using the test positions, and print comparative 
;;; results from the model and humans for all four measures.
;;;
;;; Reference:
;;;
;;; de Groot, A.D., and Gobet, F. (1996)  Perception and memory in chess. (van Gorcum)
;;;     [Please contact Fernand Gobet about obtaining a copy.]

(load "chrest-system")
(use-package :chrest)

(let ((training-data (read-scene-data "chess-positions.dat"))
      (test-data (mapcar #'construct-chess-board
                         '("r...r.k./pp.b.pbp/nq.n..p./...P..../.Pp....N/P.N.B.../..Q.BPPP/..R..RK."
                           "r....bk./..q..pp./p...p..p/.n.P.NNQ/..p...../......../PB...PPP/....R.K."
                           "..r.kb.Q/q..n.p../p.b.p.r./.p..p.../.P..P.../PNNR.Pp./..P...P./.K.B...R"
                           "r....rk./.....pp./p.bNp..p/.pP.N.q./.P...n../......../..Q.BPPP/R..R..K."
                           ".nr...k./.q..Bpp./p..p...p/.p.Pp..n/....P.../P....N.P/.PQ..PP./RB....K."
                           "....r.k./pp...p.p/..p...p./.....P../.....P../..PB..KP/P.Q...P./R.B.r.q.")))
      (relevance-values '("11111130/11020111/16222042/24432512/36534123/22426102/00101111/01110130"
                          "20000150/10104542/10226225/12242663/01222112/42221121/22000115/00002040"
                          "11204523/20040124/10121214/25455212/35145122/21112131/00100422/03010021"
                          "10212140/02020412/12631041/24236423/43412221/00020222/00204161/11110150"
                          "01520240/02206110/10432201/02355123/21322211/10200121/01600110/21000030"
                          "00001030/12000101/00102060/11113521/11124221/00114061/10104252/20443434")))
  (format t "Read ~d scenes~&" (scenes-number training-data))
  ;; train and test the two models, collecting results for each position
  (let* ((novice-model (create-chess-model training-data 5 20 200))
         (master-model (create-chess-model training-data 5 20 24000))
         (novice-results (mapcar #'(lambda (scene) (recall-scene novice-model scene))
                                 test-data))
         (master-results (mapcar #'(lambda (scene) (recall-scene master-model scene))
                                 test-data)))
    ;; output results
    (format t "~%~%Summary: ~&~&")
    (format t "Number of training positions: ~d~&" (scenes-number training-data))
    (format t "Number of test positions: ~a~&" (length test-data))
    (format t "Novice network size: ~d~&" (get-ltm-size novice-model))
    (format t "Expert network size: ~d~&" (get-ltm-size master-model))
    ;; -- experiment 1 looks at the duration of fixations
    (format t "~%Experiment 1: Duration of Fixations~%~%")
    (format t "     Human Data            Simulations~&")
    (format t "     Masters Novices       CHREST-Master CHREST-Novice~&")
    (format t "mean 260     310 ~&")
    (format t "sd   100     140 ~&")
    (format t " (times in milli-seconds)~&")
    ;; -- experiment 2 looks at the percentage of the board covered
    (format t "~%~%Experiment 2: Percentage of Board Covered~%~%")
    (format t "     Human Data            Simulations~&")
    (format t "     Masters Novices       CHREST-Master CHREST-Novice~&")
    (format t "BC   91      69 ~&")
    (format t "FBC  56      40            ~13,2F ~13,2F~&"
            (average (mapcar #'results-proportion-fixated-squares master-results))
            (average (mapcar #'results-proportion-fixated-squares novice-results)))
    (format t "PC   92      69 ~&")
    (format t "FPC  68      46            ~13,2F ~13,2F~%~%"
            (average (mapcar #'results-proportion-fixated-pieces master-results))
            (average (mapcar #'results-proportion-fixated-pieces novice-results)))
    (format t "BC - board coverage  FBC - focussed board coverage ~&")
    (format t "PC - piece coverage  FPC - focussed piece coverage ~&")
    ;; -- experiment 3 compares fixated squares against square relevance
    (format t "~%~%Experiment 3: Relevance of Squares Covered~%~%")
    (format t "     Human Data            Simulations ~&")
    (format t "     Masters Novices       CHREST-Master CHREST-Novice~&")
    (format t "ESRC 0.36    0.07 ~&")
    (format t "OSRC 0.33    0.03 ~&")
    (format t "SRC  0.36    0.08 ~%~%")
    (format t "ESRC - empty squares relevance coverage~&")
    (format t "OSRC - occupied squares relevance coverage~&")
    (format t "SRC  - square relevance coverage~&")
    ;; -- experiment 4 looks at overall recall performance
    (format t "~%~%Experiment 4: Recall Performance~%~%")
    (format t "Human Data             Simulations~&")
    (format t "Masters Novices        CHREST-Master CHREST-Novice~&")
    (format t "82.9    20.5           ~13,2F ~13,2F~&" 
            (average (mapcar #'results-recall master-results))
            (average (mapcar #'results-recall novice-results)))))

(quit)

