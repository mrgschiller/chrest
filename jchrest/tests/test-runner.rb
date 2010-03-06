# jRuby script for running CHREST test code

include Java
require "jchrest"
require "general-testing"

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

class TestPatterns
  include TestFramework

  def test_number_matches
    assert_true(@number1.equals(Pattern.make_number 1))
    assert_false(@number1.equals(@number2))
    assert_true(@number1.matches(Pattern.make_number 1))
    assert_false(@number1.matches(@number2))
  end

  def test_string_matches
    assert_true(@string1.equals(Pattern.make_string "abc"))
    assert_false(@string1.equals(@string2))
    assert_true(@string1.matches(Pattern.make_string "abc"))
    assert_false(@string1.matches(@string2))
  end

  def test_mixed_matches
    assert_false(@number1.equals(@string1))
    assert_false(@number1.matches(@string1))
    assert_false(@string1.equals(@number1))
    assert_false(@string1.matches(@number1))
  end

  def test_list_pattern_finished
    list = Pattern.make_visual_list([1,2,3,4].to_java(:int))
    assert_false(list.is_finished)
    list.set_finished
    assert_true(list.is_finished)
  end

  def test_list_pattern_equality
    list1 = Pattern.make_visual_list([1,2,3,4].to_java(:int))
    assert_false(Pattern.make_visual_list([].to_java(:int)).equals(Pattern.make_verbal_list([].to_java(:int))))
    assert_true(list1.equals(Pattern.make_visual_list([1,2,3,4].to_java(:int))))
    assert_false(list1.equals(Pattern.make_visual_list([1,2,3].to_java(:int))))
    assert_false(list1.equals(Pattern.make_visual_list([1,2,3,4,5].to_java(:int))))
    assert_false(list1.equals(Pattern.make_visual_list([1,2,4,5].to_java(:int))))
    list2 = Pattern.make_visual_list(["a","b","c","d","e"].to_java(:String))
    assert_false(list1.equals(list2))
    list3 = Pattern.make_visual_list([1,2,3,4].to_java(:int))
    list3.set_finished
    assert_false(list1.equals(list3))
    list1.set_finished
    assert_true(list1.equals(list3))
  end

  def initialize
    @number1 = Pattern.make_number 1
    @number2 = Pattern.make_number 2
    @string1 = Pattern.make_string "abc"
    @string2 = Pattern.make_string "def"

    TestFramework.add_tests(:process, self, [
                            "test_number_matches",
                            "test_string_matches",
                            "test_mixed_matches",
                            "test_list_pattern_finished",
                            "test_list_pattern_equality"
    ])
  end
end
TestPatterns.new

print "Testing Chrest: "
TestFramework.run_all

