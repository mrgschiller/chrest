This folder contains some scripts using jRuby for building and manipulating 
CHREST models.  The results are part of the standard results for CHREST, 
and more details may be found in the referred publications.

Chess: de Groot and Gobet (1996)
--------------------------------

The book by de Groot and Gobet (1996) contains detailed analysis of chess
players' eye movements and protocol analysis.  Also included is a set of
experiments using an earlier version of CHREST providing evidence of the speed
and quality of eye fixations of masters over novices, and also the qualitative
difference in recall performance of masters over novices.  

The precise training data from the book could not be precisely reproduced, but
the file '../sample-data/chess-positions.dat' contains 8644 positions, including a
range of middle game positions and opening positions, which are a close 
approximation to the 9500 positions described in the book.  

The test data were six positions from Chapter 6 of the book; these positions 
do not occur in the training set.  These positions are in the file 
'chess-test-gobet96.dat'. 

The Novice model is created by training until 200 positions are within 
the network.  The Master model is created by training on the complete 
training data for two passes.

Testing is done on the test positions, looking at four sets of data:

1. the mean and variance of the duration of eye fixations.
2. the percentage of the board covered
3. the relevance of the squares covered (relevance is measured as 
   specified in Chapter 6 of the book).
4. the recall performance on the test positions.

Note that the version of CHREST in the book included the complete set of
attention heuristics and STM mechanisms.  Also, all learning mechanisms for
building a discrimination network.  However, templates were not used for the
results in the book, leading to relatively low scores in the results for recall
performance, when compared to real master players.

The file 'test-gobet96.rb' performs the analysis as described in the book.
Running the program will construct a model using the training dataset, as 
described above, test the model using the test positions, and print 
comparative results from the model and humans for all three measures.

Chess: Master Level Performance
-------------------------------

The recall performance of masters can only be met by using the slot-value 
data structure known as 'templates', which gives the underlying theory 
of CHREST its name, the 'template theory'.  This theory is an extension 
of Chase and Simon's 'chunking theory'.  

The file 'recall-experiments.rb' performs a set of recall experiments, 
comparing networks for different skill levels.  The results obtained by 
the networks are compared with some typical values gained from experiments 
on human chess players.

TO COMPLETE

Illustrative Scripts
--------------------

This folder also contains some illustrative scripts, demonstrating 
further ways of using Chrest and the library.

1. The file 'classification.rb' creates a model for a simple classification 
task, using the weather dataset.
2. The file 'demo-1.rb' illustrates some of the different methods for 
creating and interacting with models.
3. The file 'demo-2.rb' further illustrates Chrest learning, and shows how 
to save out a series of images of the learning process.
4. The file 'demo-3.rb' explores the training of very large networks from 
patterns of random numbers.
5. The file 'order-effects.rb' runs the experiments from Chapter 5 
of Ritter et al (2007).
6. The file 'verbal-learning-bugleski.rb' runs the verbal learning experiment 
of Bugelski (1962), demonstrating that the number of training cycles was 
inversely proportional to the amount of time each item was seen.  (TO COMPLETE)

References
----------

de Groot, A.D., and Gobet, F. (1996)  Perception and memory in chess. 
   van Gorcum.  [Please contact Fernand Gobet about obtaining a copy.]
Ritter, F, et. al. (2007)  Order Effects in Learning.
