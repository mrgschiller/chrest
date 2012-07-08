// Example 1 : Groovy script
//
// In this example, we create some instances of patterns,
// train a Chrest model with them, and then print out 
// and display what Chrest has learnt.
//
// To run this script, you must have installed groovy, 
// and have jchrest.jar and example1.groovy together in 
// the same directory.  Then type: 
// groovy -classpath jchrest.jar example1.groovy
//
// or start 'groovyConsole', open up this file within it,
// and add jchrest.jar to the Classpath (under the 'Script' menu)
// Finally, 'run script' (under the 'Script' menu).

import jchrest.architecture.Chrest
import jchrest.gui.ChrestView
import jchrest.lib.Pattern

// Create an instance of the Chrest model
def model = new Chrest ();

// Create three pattern instances
def pattern1 = Pattern.makeList([1, 2, 3] as int[]);
def pattern2 = Pattern.makeList([1, 3, 2] as int[]);
def pattern3 = Pattern.makeList([2, 1, 3] as int[]);

// Store them in an array
def patterns = [pattern1, pattern2, pattern3]

// Train the model a few times on the patterns
for (i in (1..4)) {
  for (pat in patterns) {
    model.recogniseAndLearn(pat);
  }
}

// Display the results
println "Current model time: " + model.getClock ();
for (pat in patterns) {
  print "For pattern: " + pat.toString () + " model retrieves ";
  println model.recallPattern(pat).toString ();
}

// And display the Model in a graphical view
new ChrestView (model);
