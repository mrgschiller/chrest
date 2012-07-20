# Chrest tests for patterns

process_test "number matches" do
  number1 = Pattern.makeNumber 1
  number2 = Pattern.makeNumber 2

  assert_true number1.equals(NumberPattern.create(1))
  assert_false number1.equals(number2)
  assert_true number1.matches(NumberPattern.create(1))
  assert_false number1.matches(number2)
end

process_test "string matches" do
  string1 = Pattern.makeString "abc"
  string2 = Pattern.makeString "def"

  assert_true string1.equals(StringPattern.create("abc"))
  assert_false string1.equals(string2)
  assert_true string1.matches(StringPattern.create("abc"))
  assert_false string1.matches(string2)
end

process_test "ios matches" do
  ios1 = ItemSquarePattern.new("P", 2, 3)
  ios1a = ItemSquarePattern.new("P", 2, 3)
  ios2 = ItemSquarePattern.new("Q", 2, 3)

  assert_true ios1.equals(ios1a)
  assert_false ios1.equals(ios2)
  assert_true ios1.matches(ios1a)
  assert_false ios1.matches(ios2)
end

process_test "mixed matches" do
  number = Pattern.makeNumber 1
  string = Pattern.makeString "abc"

  assert_false number.equals(string)
  assert_false number.matches(string)
  assert_false string.equals(number)
  assert_false string.matches(number)
end

process_test "list finished" do
  lp = Pattern.makeVisualList([1,2,3,4].to_java(:int))

  assert_false lp.isFinished
  lp.setFinished
  assert_true lp.isFinished
end

process_test "list equality" do
  lp = Pattern.makeVisualList([1,2,3,4].to_java(:int))
  lp2 = Pattern.makeVisualList(["a", "b", "c", "d", "e"].to_java(:String))

  assert_false Pattern.makeVisualList([].to_java(:int)).equals(Pattern.makeVerbalList([].to_java(:int)))
  assert_true(lp.equals(Pattern.makeVisualList([1,2,3,4].to_java(:int))), "simple equals")
  assert_false(lp.equals(Pattern.makeVisualList([1,2,3].to_java(:int))))
  assert_false(lp.equals(Pattern.makeVisualList([1,2,3,4,5].to_java(:int))))
  assert_false(lp.equals(Pattern.makeVisualList([1,2,3,5].to_java(:int))))
  assert_false(lp.equals(lp2))

  lp_copy = Pattern.makeVisualList([1,2,3,4].to_java(:int))
  lp_copy.setFinished

  assert_false(lp.equals(lp_copy))
  lp.setFinished
  assert_true(lp.equals(lp_copy))
end

process_test "list equality for ios" do
  lp1 = ListPattern.new
  lp1.add ItemSquarePattern.new("P", 2, 3)
  lp2 = ListPattern.new
  lp2.add ItemSquarePattern.new("Q", 2, 3)
  lp3 = ListPattern.new
  lp3.add ItemSquarePattern.new("P", 2, 3)

  assert_true(lp1.equals(lp3))
  assert_false(lp1.equals(lp2))
end

process_test "list pattern matches 1" do
  lp1 = Pattern.makeVisualList([1,2,3,4].to_java(:int))
  lp2 = Pattern.makeVisualList(["a", "b", "c", "d", "e"].to_java(:String))
  lp3 = Pattern.makeVisualList([1,2,3].to_java(:int))

  assert_false Pattern.makeVisualList([].to_java(:int)).matches(Pattern.makeVerbalList([].to_java(:int)))
  assert_true ListPattern.new.matches(lp1)
  assert_true lp1.matches(lp1)
  assert_false lp1.matches(lp2)
  assert_true lp3.matches(lp1)
  assert_false lp1.matches(lp3)
end

process_test "list pattern matches 2" do
  lp1 = Pattern.makeVisualList([1,2,3,4].to_java(:int))
  prim1 = Pattern.makeVisualList([1].to_java(:int))

  assert_true prim1.matches(lp1)

  prim1_clone = prim1.clone
  prim1_clone.setFinished

  assert_true prim1.matches(prim1_clone)
  assert_false prim1_clone.matches(prim1)
end

process_test "list pattern matches 3" do
  empty = ListPattern.new
  lp1 = Pattern.makeVisualList([1,2,3,4].to_java(:int))

  assert_true empty.matches(lp1)
  empty.setFinished

  assert_false empty.matches(lp1)
  assert_true empty.matches(empty)
end

process_test "list pattern append" do
  lp1 = Pattern.makeVisualList([1,2,3,4].to_java(:int))
  lp3 = Pattern.makeVisualList([1,2,3].to_java(:int))

  assert_true(lp1.equals(lp3.append(Pattern.makeVisualList([4].to_java(:int)))))
  assert_true(lp1.equals(lp3.append(Pattern.makeNumber(4))))
end

process_test "list pattern remove" do
  lp1 = Pattern.makeVisualList([1,2,3,4].to_java(:int))
  lp3 = Pattern.makeVisualList([1,2,3].to_java(:int))

  assert_true Pattern.makeVisualList([4].to_java(:int)).equals(lp1.remove(lp3))
  assert_true Pattern.makeVisualList([4].to_java(:int)).equals(lp1.remove(Pattern.makeVisualList([1,2,3,5,6].to_java(:int))))

  pattern = lp3.clone
  pattern.setFinished

  assert_true pattern.remove(lp3).isEmpty
  assert_true pattern.remove(lp3).isFinished
end
