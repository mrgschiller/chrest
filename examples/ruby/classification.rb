# Simple test that the model can handle a classification problem

require "java"
require "chrest"

include_class "jchrest.architecture.Chrest"
include_class "jchrest.gui.ChrestView"
include_class "jchrest.gui.ChrestLtmView"
include_class "jchrest.gui.Orientation"
include_class "jchrest.lib.Pattern"

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

def make_name_pattern str
  pat = Pattern.makeVerbalList([str].to_java(:String))
  pat.setFinished
  pat
end

@@model = Chrest.new

Weather = [ ["sunny", "hot", "high", "false", "no"],
          ["sunny", "hot", "high", "true", "no"],
          ["overcast", "hot", "high", "false", "yes"],
          ["rainy", "mild", "high", "false", "yes"],
          ["rainy", "cool", "normal", "false", "yes"],
          ["rainy", "cool", "normal", "true", "no"],
          ["overcast", "cool", "normal", "true", "yes"],
          ["sunny", "mild", "high", "false", "no"],
          ["sunny", "cool", "normal", "false", "yes"],
          ["rainy", "mild", "normal", "false", "yes"],
          ["sunny", "mild", "normal", "true", "yes"],
          ["overcast", "mild", "high", "true", "yes"],
          ["overcast", "hot", "normal", "false", "yes"],
          ["rainy", "mild", "high", "true", "no"] ]

def construct_patterns data
  data.collect {|item| [make_string_pattern(["outlook-#{item[0]}", "temperature-#{item[1]}", "humidity-#{item[2]}", "windy-#{item[3]}"]) , make_name_pattern(item[4])]}
end

Patterns = construct_patterns Weather

12.times do |i|
  for pair in Patterns
    @@model.learnAndNamePatterns(pair[0], pair[1])
  end
  print "Performance on cycle #{i} is: "
  sum = 0
  for pair in Patterns
    if @@model.namePattern(pair[0]) != nil
      sum += 1 if @@model.namePattern(pair[0]).matches(pair[1]) # TODO 'equals' fails ??
    end
  end
  puts "#{sum} / #{Patterns.length}"
end

@@view = ChrestView.new(nil, @@model)
