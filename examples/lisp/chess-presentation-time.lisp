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

;;; Chess: Gobet and Simon (2000)
;;; -----------------------------
;;; 
;;; The paper by Gobet and Simon (2000) presents some experiments on chess 
;;; players of three different skill levels (class-A, Expert and Master), 
;;; looking at their ability to recall game and random positions.  The 
;;; gathered data were compared with a version of CHREST, trained to emulate 
;;; the memory of chess players at the three different skill levels.
;;;
;;; WHAT TRAINING DATA?
;;; 
;;; The test data were made from fifty positions not used for training: these 
;;; were presented directly as game positions, and their pieces randomly located 
;;; for the random positions. 
;;; 
;;; As in the paper, three models were created:
;;;   - the class-A-model with 500 chunks
;;;   - the expert-model with 10,000 chunks
;;;   - the master-model with 300,000 chunks
;;; 
;;; Testing uses the 100 test positions, presented for 1, 2, 3, 4, 5, 10, 20, 
;;; 30, and 60 seconds each, looking at:
;;; 
;;; 1. recall performance on the test positions.
;;; 2. number of chunks used in reconstructing each position.
;;; 3. size of chunks used in reconstructing each position.
;;; 4. errors of omission and commission.
;;; 
;;; Running the following program will construct CHREST models as described 
;;; above, test the models using the test positions, and print comparative 
;;; results from the model and humans for all the above measures. 
;;;
;;; Reference:
;;;
;;; F. Gobet and H. A. Simon, 'Five seconds or sixty? Presentation time in 
;;; expert memory', Cognitive Science, 24:651-682, 2000.

(load "chrest-system")
(use-package :chrest)

(let ((training-data (read-scene-data "chess-positions.dat"))
      (test-data ()))
  (format t "Read ~d scenes~&" (scenes-number training-data))
  ;; train the three models
  (let ((class-a-model (create-chess-model training-data 5 20 500))
        (expert-model (create-chess-model training-data 5 20 10000))
        (master-model (create-chess-model training-data 50 20 50000))) ; 300000)))
    ;; output results
    (format t "~%~%Summary: ~&~&")
    (format t "Number of training positions: ~d~&" (scenes-number training-data))
    (format t "Class A network size: ~d~&" (get-ltm-size class-a-model))
    (format t "Expert network size: ~d~&" (get-ltm-size expert-model))
    (format t "Master network size: ~d~&" (get-ltm-size master-model))
    ;; -- experiment 1 looks at overall recall performance
    (format t "~%~%Experiment 1: Recall Performance~%~%")
    (format t "Game Positions:~%")
    (format t "Time  Human Data               Simulations~&")
    (format t " (s)  Class A  Expert  Master  Class A  Expert  Master~&")
    (format t "  1      25.3    33.1    67.1 ~&" )
    (format t "  2      29.8    38.8    88.1 ~&" )
    (format t "  3      28.0    41.4    89.7 ~&" )
    (format t "  4      36.4    46.9    92.8 ~&" )
    (format t "  5      32.4    51.9    92.4 ~&" )
    (format t " 10      38.0    66.3      -  ~&" )
    (format t " 20      57.9    83.5      -  ~&" )
    (format t " 30      79.3    91.5      -  ~&" )
    (format t " 60      88.1    97.2      -  ~&" )
    (format t "~%Random Positions:~%")
    (format t "Time  Human Data               Simulations~&")
    (format t " (s)  Class A  Expert  Master  Class A  Expert  Master~&")
    (format t "  1       9.4    11.4    15.2 ~&" )
    (format t "  2      12.2    15.8    16.5 ~&" )
    (format t "  3       8.9    15.9    17.6 ~&" )
    (format t "  4      10.6    19.7    25.8 ~&" )
    (format t "  5      13.6    20.6    33.8 ~&" )
    (format t " 10      15.4    18.2    33.3 ~&" )
    (format t " 20      17.4    31.6    48.0 ~&" )
    (format t " 30      20.6    51.4    51.1 ~&" )
    (format t " 60      40.1    50.4    67.6 ~&" )
    ))

(quit)

;;; ----------------------------
;;; Below is the raw data for human performances
;;;
;;; first sublist is for game positions, the second for random positions
;;; second sublist is by level: class-A, Experts, IM/GM
#|
(defvar *human-template*
  '(
    ((-1 -1 -1 -1 -1 -1 -1 -1  -1)
     (-1 -1 -1 -1 -1 -1 -1 -1  -1)
     (-1 -1 -1 -1 -1 -1 -1 -1  -1))

    ((-1 -1 -1 -1 -1 -1 -1 -1  -1)
     (-1 -1 -1 -1 -1 -1 -1 -1  -1)
     (-1 -1 -1 -1 -1 -1 -1 -1  -1))))

(defvar *human-percentage*
  '(
    ((25.3 29.8 28.0 36.4 32.4 38.0 57.9 76.3  88.1)
     (33.1  38.8  41.4  46.9  51.9  66.3  83.5  91.5  97.2)
     (67.1  88.1  89.7  87.8  92.8  92.4  -1  -1  -1))
    ((9.4  12.2 8.9 10.6 13.6 15.4 17.4 20.6 40.1)
     (11.4 15.8 15.9 19.7 20.6 18.2 31.6 51.4 50.4)
     ( 15.2 16.5 17.6 25.8 33.8 33.3 48.0 51.1 67.6 ))))


(defvar *human-size-of-chunks*
  '(
    ((5.42 6.67 6.42 8.83 7.07 5.71 8.86 8.93 10.57) 
     (9.5 11.13 9.63 10.44 11.94 12.69 13.75 14.38 17.56)
     (13 14.6 12.8 16.5 14 16.1 -1 -1 -1))
    ((3 4.83 3.33 3.83 4.86 4.86 4.14 4.71 5.86)
     (4.63 5 5.25 5.88 5.75 5.88 6.38 6.75 7.75)
     ( 5 5.75 5.25 6.25 5.5 6.25 8.25 6 12))))

( defvar *human-number-of-chunks*      
         '(
           ((2.2	2.2	2.6	2.7	2.7	3.9	3.9	5.1   5.4)
            (2.2	2.3	2.6	3.4	2.9	4.5	3.9	3.6	2.9)
            (2.9	3.6	3.7	4.2	2.9	3.3	-1	-1	-1  ))
           ((1.2	1.2	1.5	1.8	1.7	2.1	2.3	2.1	4.3)
            (1.4	1.2	1.4	1.9	1.2	2.6	2.9	4.2	4.5)
            (1.8	1.2	1.5	1.8	1.8	2.2	3.5	5.5	3.2))))
|#
