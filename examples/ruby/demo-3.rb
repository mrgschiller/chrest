# Construct a large tree
#
# java -server -Xmx2500M -jar jruby.jar training-large.rb 100000 50
# produces a 91917 node network in 127 seconds.
# Without the '-server' option, it takes 210 seconds.

# java -server -Xmx2500M -jar jruby.jar training-large.rb 500000 50
# produces a 455601 node network in 8 minutes 31 seconds.

require "java"
require "chrest"

include_class "jchrest.architecture.Chrest"
include_class "jchrest.gui.ChrestView"
include_class "jchrest.gui.ChrestLtmView"
include_class "jchrest.gui.Orientation"
include_class "jchrest.lib.Pattern"

include_class "javax.swing.JFileChooser"

# Convenience function to convert a ruby array of integers into 
# Java ArrayList<int>, before passing it to Pattern to construct 
# a ListPattern
def make_number_pattern array
  pat = Pattern.makeVisualList(array.to_java(:int))
  pat.setFinished
  pat
end

def make_string_pattern array
  pat = Pattern.makeVisualList(array.to_java(:String))
  pat.setFinished
  pat
end

# Create an instance of the Chrest model and a view
@@model = Chrest.new

Patterns = []

# create a large pool of patterns by constructing lists of random 
# numbers.

@@num_patterns = ARGV[0].to_i  # total number of patterns to make
@@max_length = ARGV[1].to_i    # maximum length of largest pattern

@@num_patterns.times do 
  pattern = []
  rand(@@max_length).times do 
    pattern << rand(10)
  end
  Patterns << make_number_pattern(pattern)
end

for pattern in Patterns
  pattern.size.times do
    @@model.recogniseAndLearn pattern
  end
end

if @@model.ltmSize < 50000 # TODO try to resolve this limit on display size
  @@view = ChrestView.new(nil, @@model)
end

puts "Network contains #{@@model.ltmSize} nodes"
