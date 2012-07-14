# Overall tests of Chrest

process_test "timings" do
  model = Chrest.new
  model.setRho 1.0
  model.setFamiliarisationTime 2000
  model.setDiscriminationTime 10000
  patternA = Pattern.makeVisualList(["B", "I", "F"].to_java(:String))
  patternB = Pattern.makeVisualList(["X", "A", "Q"].to_java(:String))

  assert_equal(0, model.getClock)
  # check changed on one learning operation
  model.recogniseAndLearn patternA # -- discriminate node for 'B'
  assert_equal(10000, model.getClock)
  # check changed on second learning operation
  model.recogniseAndLearn patternA # -- familiarise node for 'B'
  assert_equal(12000, model.getClock)
  # check a busy model is not changed
  model.recogniseAndLearn(patternB, 10000) # -- busy, no change
  assert_equal(12000, model.getClock)
  model.recogniseAndLearn patternA # -- discriminate node for 'I'
  assert_equal(22000, model.getClock)
  model.recogniseAndLearn patternA # -- familiarise node with 'BI'
  assert_equal(24000, model.getClock)
  model.recogniseAndLearn patternA # -- discriminate node for 'F'
  assert_equal(34000, model.getClock)
  model.recogniseAndLearn patternA # -- familiarise node for 'BIF'
  assert_equal(36000, model.getClock)
  model.recogniseAndLearn patternA # -- no change, pattern fully learnt
  assert_equal(36000, model.getClock)
end

process_test "base case" do
  model = Chrest.new
  emptyList = Pattern.makeVisualList([].to_java(:int))
  assert_true(Pattern.makeVisualList(["Root"].to_java(:String)).equals(model.recognise(emptyList).getImage))
end

process_test "learning case 1" do
  # Every item that is learnt must first be learnt at the top-level,
  # as a primitive.  Learning that top-level node is done with an empty image.
  model = Chrest.new
  emptyList = Pattern.makeVisualList([].to_java(:int))
  list = Pattern.makeVisualList([1,2,3,4].to_java(:int))
  list.setFinished
  prim = Pattern.makeVisualList([1].to_java(:int))
  prim_test = Pattern.makeVisualList([1].to_java(:int))
  prim.setFinished

  model.recogniseAndLearn list
  assert_equal(1, model.getLtmByModality(list).getChildren.size)

  firstChild = model.getLtmByModality(list).getChildren.get(0)
  assert_false(emptyList.equals(firstChild.getChildNode.getContents))
  assert_true(firstChild.getTest.equals(prim_test))
  assert_true(firstChild.getChildNode.getContents.equals(prim_test))
  assert_true(firstChild.getChildNode.getImage.equals(emptyList))
end

process_test "learning case 2" do
  # Same as 'learning case 1', but using item-on-square instead of simple numbers
  model = Chrest.new
  emptyList = ListPattern.new
  list = ListPattern.new
  list.add ItemSquarePattern.new("P", 1, 2)
  list.add ItemSquarePattern.new("P", 2, 2)
  list.add ItemSquarePattern.new("P", 3, 2)
  list.add ItemSquarePattern.new("P", 4, 2)
  list.setFinished
  prim= ListPattern.new
  prim.add ItemSquarePattern.new("P", 1, 2)
  prim_test = prim.clone
  prim.setFinished

  model.recogniseAndLearn list
  assert_equal(1, model.getLtmByModality(list).getChildren.size)

  firstChild = model.getLtmByModality(list).getChildren.get(0)
  assert_false(emptyList.equals(firstChild.getChildNode.getContents))
  assert_true(firstChild.getTest.equals(prim_test))
  assert_true(firstChild.getChildNode.getContents.equals(prim_test))
  assert_true(firstChild.getChildNode.getImage.equals(emptyList))
end

process_test "simple retrieval 1" do
  # Check that after learning a primitive, the model will retrieve 
  # that node on trying to recognise the list
  model = Chrest.new
  list = Pattern.makeVisualList([1,2,3,4].to_java(:int))
  list.setFinished
  emptyList = Pattern.makeVisualList([].to_java(:int))
  prim = Pattern.makeVisualList([1].to_java(:int))
  prim_test = Pattern.makeVisualList([1].to_java(:int))
  prim.setFinished

  model.recogniseAndLearn list
  node = model.recognise list

  assert_false emptyList.equals(node.getContents)
  assert_true prim_test.equals(node.getContents)
  assert_true emptyList.equals(node.getImage)
end

process_test "simple learning 2" do
  model = Chrest.new
  list = Pattern.makeVisualList([1,2,3,4].to_java(:int))
  list2 = Pattern.makeVisualList([2,3,4].to_java(:int))
  list3 = Pattern.makeVisualList([1,3,4].to_java(:int))
  list3_test = Pattern.makeVisualList([1,3].to_java(:int))
  emptyList = Pattern.makeVisualList([].to_java(:int))
  prim1 = Pattern.makeVisualList [1].to_java(:int)
  prim2 = Pattern.makeVisualList [2].to_java(:int)

  model.recogniseAndLearn list2
  model.recogniseAndLearn list
  assert_equal(2, model.getLtmByModality(list).getChildren.size)
  # check most recent becomes the first child node
  assert_true prim1.equals(model.getLtmByModality(list).getChildren.get(0).getChildNode.getContents)
  assert_true prim2.equals(model.getLtmByModality(list).getChildren.get(1).getChildNode.getContents)
  # force discriminate from node 0
  # by first overlearning
  model.recogniseAndLearn list
  model.recogniseAndLearn list
  assert_true model.recognise(list).getImage.equals(Pattern.makeVisualList([1,2].to_java(:int)))
  node = model.getLtmByModality(list).getChildren.get(0).getChildNode
  assert_equal(0, node.getChildren.size)
  model.recogniseAndLearn list3 # first learn the '3' to use as test
  model.recogniseAndLearn list3 # now trigger discrimination
  assert_equal(1, node.getChildren.size)
  assert_true list3_test.equals(node.getChildren.get(0).getChildNode.getImage)
  assert_true list3_test.equals(node.getChildren.get(0).getChildNode.getContents)
  # and familiarise
  node = node.getChildren.get(0).getChildNode
  model.recogniseAndLearn list3
  model.recogniseAndLearn list3
  assert_true list3.equals(node.getImage)
end

process_test "check learning of < $ >" do
  model = Chrest.new
  list1 = Pattern.makeVisualList(["A", "B", "C"].to_java(:String))
  list2 = Pattern.makeVisualList(["A", "B"].to_java(:String))
  8.times do 
    model.recogniseAndLearn list1
  end
  assert_true list1.equals(model.recallPattern(list1))
  assert_true list1.equals(model.recallPattern(list2))
  node = model.recognise list2
  assert_true list1.equals(node.getImage)
  # learning should result in discrimination with < $ >
  model.recogniseAndLearn list2
  assert_equal(1, node.getChildren.size)
end

process_test "full learning" do 
  model = Chrest.new
  list1 = Pattern.makeVisualList([3,4].to_java(:int))
  list2 = Pattern.makeVisualList([1,2].to_java(:int))

  20.times do 
    model.recogniseAndLearn list1
    model.recogniseAndLearn list2
  end

  assert_true list1.equals(model.recallPattern(list1))
  assert_true list2.equals(model.recallPattern(list2))
end
