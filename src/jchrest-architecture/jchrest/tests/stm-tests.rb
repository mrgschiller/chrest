# Test use of STM

# ensure empty LTM results in root node being placed in STM on
# trying to recognise a pattern
process_test "stm tests 1" do
  model = Chrest.new
  patternA = Pattern.makeVisualList(["A", "E", "F"].to_java(:String))

  assert_equal(0, model.getVisualStm.getCount)
  model.recognise patternA
  assert_equal(1, model.getVisualStm.getCount)
  assert_true(model.getLtmByModality(patternA) == model.getVisualStm.getItem(0))
end

# ensure learning an item results in STM containing two nodes:
# the first is the new node, and the second is the root node
process_test "stm tests 2" do
  model = Chrest.new
  patternA = Pattern.makeVisualList(["A", "E", "F"].to_java(:String))

  model.recogniseAndLearn patternA
  assert_equal(2, model.getVisualStm.getCount)
  assert_true(model.getLtmByModality(patternA) == model.getVisualStm.getItem(1))
end

# ensure capacity of STM is respected
process_test "stm tests: capacity" do
  model = Chrest.new
  patternA = Pattern.makeVisualList(["A", "E", "F"].to_java(:String))
  patternB = Pattern.makeVisualList(["B", "E", "F"].to_java(:String))
  patternC = Pattern.makeVisualList(["C", "E", "F"].to_java(:String))
  patternD = Pattern.makeVisualList(["D", "E", "F"].to_java(:String))

  model.recogniseAndLearn patternA
  assert_equal(2, model.getVisualStm.getCount)
  model.recogniseAndLearn patternB
  assert_equal(3, model.getVisualStm.getCount)
  model.recogniseAndLearn patternC
  assert_equal(4, model.getVisualStm.getCount)
  model.recogniseAndLearn patternD
  assert_equal(4, model.getVisualStm.getCount)
  model.recogniseAndLearn patternA
  assert_equal(4, model.getVisualStm.getCount)
end

# ensure a new item, if repeated, is correctly placed into STM
process_test "stm test: repeated item" do
  model = Chrest.new
  patternA = Pattern.makeVisualList(["A", "E", "F"].to_java(:String))
  patternB = Pattern.makeVisualList(["B", "E", "F"].to_java(:String))
  patternC = Pattern.makeVisualList(["C", "E", "F"].to_java(:String))
  patternD = Pattern.makeVisualList(["D", "E", "F"].to_java(:String))

  model.recogniseAndLearn patternA
  node = model.getVisualStm.getItem 0
  model.recogniseAndLearn patternB
  model.recogniseAndLearn patternC
  # check node occurs only once in the STM, and at the fourth place down
  assert_equal(node, model.getVisualStm.getItem(3))
  count = 0
  model.getVisualStm.getSize.times do |i|
    count += 1 if node == model.getVisualStm.getItem(i)
  end
  assert_equal(1, count)
  # check newly recognised node occurs only once int he STM, and at first place
  model.recognise patternA
  assert_equal(node, model.getVisualStm.getItem(0))
  count = 0
  model.getVisualStm.getSize.times do |i|
    count += 1 if node == model.getVisualStm.getItem(i)
  end
  assert_equal(1, count)
end
