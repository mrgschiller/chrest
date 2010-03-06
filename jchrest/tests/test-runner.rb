# jRuby script for running CHREST test code

include Java
require "jchrest"

include_class "jchrest.lib.NumberPattern"
include_class "jchrest.lib.Pattern"

# Convenience function to convert a ruby array of integers into 
# Java ArrayList<int>, before passing it to Pattern to construct 
# a ListPattern
def make_number_pattern array
  pat = Pattern.makeVisualList(array.to_java(:int))
  pat.setFinished
  pat
end

def assert_true expr
  if expr
    print "."
  else
    puts "Error"
  end
end


def test_patterns
  number1 = make_number_pattern [1]
  assert_true(number1.equals(make_number_pattern([1])))
#  equal_lists = number1.java_method(:equals, [Java::jchrest.lib.Pattern])
#  assert_true(equal_lists.call make_number_pattern([1]))
end

def run_all
  print "Testing CHREST: "
  test_patterns
  puts ""
  puts " DONE"
end

run_all

