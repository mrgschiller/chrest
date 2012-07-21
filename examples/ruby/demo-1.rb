# Demonstration 1 : jRuby script
#
# In this example, we create some instances of patterns,
# train a Chrest model with them, and then print out 
# and display what Chrest has learnt.
#
# To run this script you will need jruby.jar, jchrest.jar and 
# example-1.rb together in the same directory.  Then type:
# java -jar jruby.jar example-1.rb

require "java"
require "chrest"

include_class ["jchrest.architecture.Chrest", "jchrest.gui.ChrestView", "jchrest.lib.Pattern"]

# Convenience function to convert a ruby array of integers into 
# Java ArrayList<int>, before passing it to Pattern to construct 
# a ListPattern
def make_number_pattern array
  Pattern.makeVisualList(array.to_java(:int))
end

# Create an instance of the Chrest model
model = Chrest.new

# Create three pattern instances
pattern1 = make_number_pattern [1, 2, 3]
pattern2 = make_number_pattern [1, 3, 2]
pattern3 = make_number_pattern [2, 1, 3]

# store them in an array 
Patterns = [pattern1, pattern2, pattern3]

# Train the model a few times on the patterns
4.times do 
  for pat in Patterns
    model.recogniseAndLearn pat
  end
end

# Display the results
puts "Current model time: #{model.getClock}"
for pat in Patterns
  print "For pattern: #{pat.toString} model retrieves "
  puts "#{model.recallPattern(pat).toString}"
end

# And display the Model in a graphical view
ChrestView.new(model)

