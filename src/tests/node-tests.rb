# Test methods of Node

process_test "node information 1" do
  node = Node.new(Chrest.new, 0, Pattern.makeVisualList([].to_java(:String)))
  assert_equal(0, node.information)
end

process_test "node information 2" do
  node = Node.new(
    Chrest.new, 
    0, 
    Pattern.makeVisualList([].to_java(:String))
  )
  node.image = Pattern.makeVisualList(["A", "B"].to_java(:String))
  assert_equal(0, node.information) # root node always 0
end

process_test "node information 3" do
  node = Node.new(
    Chrest.new, 
    Pattern.makeVisualList(["A"].to_java(:String)),
    Pattern.makeVisualList(["A", "B"].to_java(:String))
  )
  assert_equal(2, node.information) 
end

