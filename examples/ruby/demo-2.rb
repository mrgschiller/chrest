# Demonstration 2 : jRuby script
#
# In this example, we create some instances of patterns,
# train a Chrest model with them, and save out a sequence 
# of images which show the gradual construction of the 
# network.
#
# To run this script you will jruby.jar, jchrest.jar and 
# demo-2.rb together in the same directory.  Then type:
# java -jar jruby.jar demo-2.rb

require "java"
require "jchrest"

import "jchrest.architecture.Chrest"
import "jchrest.gui.ChrestView"
import "jchrest.gui.ChrestLtmView"
import "jchrest.lib.Pattern"

import "javax.swing.JFileChooser"

# Convenience function to convert a ruby array of integers into 
# Java ArrayList<Integer>, before passing it to Pattern to construct 
# a ListPattern
def make_number_pattern array
  Pattern.makeVisualList(array.to_java(:int))
end

# Convenience function to convert a ruby array of Strings into 
# Java ArrayList<String>, before passing it to Pattern to construct 
# a ListPattern
def make_string_pattern array
  pat = Pattern.makeVisualList(array.to_java(:String))
  pat.setFinished
  pat
end

@@num = 1 # a global count of the pictures saved, to keep filenames unique
def learnAndSave(pattern, base="demo", cycles=1)
  cycles.times do
    @@model.recogniseAndLearn pattern
    @@view.saveLongTermMemory(java::io::File.new("#{base}#{"%03d" % @@num}.png"))
    @@num += 1
  end
end

# Create an instance of the Chrest model and a view
@@model = Chrest.new
@@view = ChrestView.new(nil, @@model)

# Run example 1
# -------------
# Create three pattern instances
pattern1 = make_number_pattern [1, 2, 3]
pattern2 = make_number_pattern [1, 3, 2]
pattern3 = make_number_pattern [2, 1, 3]

# store them in an array 
Patterns = [pattern1, pattern2, pattern3]

# Train the model a few times on the patterns
@@num = 1
4.times do 
  for pattern in Patterns
    learnAndSave(pattern, "image")
  end
end

# Reset and run example 2
# -----------------------

@@model.clear
@@num = 1
learnAndSave(make_string_pattern(["A", "B", "C"]), "demo", 8)
learnAndSave(make_string_pattern(["A", "B"]), "demo", 4)
learnAndSave(make_string_pattern(["D", "E", "A", "B"]), "demo", 7)

@@view.setVisible false
@@view.dispose
