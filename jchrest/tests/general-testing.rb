# Generalised Testing Framework
# written by Peter Lane, April 2008

# ABOUT THIS SOFTWARE
#
# The functions in this file support use of a testing framework with
# tests divided into groups.  An example application is for testing 
# scientific theories, where three classes may be used, as 
# described in [1]:
# 1. Unit tests - for implementational details
# 2. Process tests - for the main processes within the theory
# 3. Canonical results - for the experimental results achieved by the
#                        theory
# [1] P.C.R. Lane and F. Gobet, Developing reproducible and comprehensible
#     computational models, Artificial Intelligence, 144:251--63, 2003.

# INSTRUCTIONS FOR USE
#
# Implement your tests in a class, and include the TestFramework module
#
# Individual tests are written in one of the following forms:
# test(THING-TO-CHECK [, MESSAGE])
#     where THING-TO-CHECK is a test, returning a boolean value
#                          test should return true if the test passed
#           MESSAGE is optional, and is displayed if the test returns false
# assert_equals(EXPECTED, ACTUAL [, MESSAGE])
# assert_true(THING-TO-CHECK [, MESSAGE])
#    the same as test(THING-TO-CHECK [, MESSAGE])
# assert_false(THING-TO-CHECK [, MESSAGE])
#    the same as test(!THING-TO-CHECK [, MESSAGE])

# To create a group of tests, you add the names of the tests to a group 
# in the constructor of the test class. 
# e.g. TestFramework.addTests(:unit, self, ["x", "x2", "x3"])

# Tests can be run using either of:
# TestFramework.run_all   which runs the tests in each group in turn
# TestFramework.run_group(group-name)  which runs the tests in the named group

# TODO: Improve syntax for adding tests

module TestFramework
  @@store = Hash.new

  def TestFramework.reset_counts
    @@errors = 0
    @@total_tests = 0
  end

  def TestFramework.feedback_counts
    puts "\n=== DONE: There #{@@errors==1 ? "was" : "were"} #{@@errors} error#{@@errors == 1 ? "" : "s"} in #{@@total_tests} test#{@@total_tests == 1 ? "" : "s"}."
  end

  # -- individual test functions
  def test(val, msg="")
    @@total_tests += 1
    if val then
      print "."
    else
      @@errors += 1
      puts "\nError #{@@errors}: #{msg}" 
    end
  end

  def assert_true(val, msg="")
    test(val, msg)
  end

  def assert_false(val, msg="")
    test(!val, msg)
  end

  def assert_equals(exp, val, msg="")
    test(exp == val, "Expected #{exp} got #{val}. #{msg}")
  end

  # -- placing functions into groups
  def TestFramework.group(group, *functions)
    (@@store[group] ||= []).push *functions 
  end

  def TestFramework.add_tests(test_group, obj, names)
    names.each {|n| TestFramework.group(test_group, obj.method(n))}
  end

  # -- running tests in each group
  def TestFramework.run_all
    @@store.each_key {|key| TestFramework.run_group key}
  end

  def TestFramework.run_group(group)
    TestFramework.reset_counts
    if @@store.has_key? group then
      print "\nRunning #{group} tests: "
      for fn in @@store[group]
        fn.call
      end
    end
    TestFramework.feedback_counts
  end
end

