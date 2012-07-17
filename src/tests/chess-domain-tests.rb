# Ruby test suite for Chrest: Chess Domain Tests

unit_test "piece types" do 
  domain = ChessDomain.new 
  board = ChessDomain.constructBoard("......../......../....N.../......../.......R/R...k.../....P.../N...R.p.")

  big_pieces = domain.getBigPieces(board)
  assert_equal(6, big_pieces.size)
  assert_true(big_pieces.any?{|s| s.row == 7 and s.column == 0})
  assert_true(big_pieces.any?{|s| s.row == 7 and s.column == 4})
  assert_true(big_pieces.any?{|s| s.row == 5 and s.column == 0})
  assert_true(big_pieces.any?{|s| s.row == 5 and s.column == 4})
  assert_true(big_pieces.any?{|s| s.row == 4 and s.column == 7})
  assert_true(big_pieces.any?{|s| s.row == 2 and s.column == 4})

  offensive_pieces = domain.getOffensivePieces(board)
  assert_equal(3, offensive_pieces.size)
  assert_true(offensive_pieces.any?{|s| s.row == 7 and s.column == 6})
  assert_true(offensive_pieces.any?{|s| s.row == 2 and s.column == 4})
  assert_true(offensive_pieces.any?{|s| s.row == 2 and s.column == 4})
end

unit_test "normalisation of item-square-pattern lists" do
  lp = ListPattern.new
  lp.add ItemSquarePattern.new("k", 2, 3)
  lp.add ItemSquarePattern.new("P", 4, 2)
  lp.add ItemSquarePattern.new("q", 2, 3)

  assert_equal(3, lp.size)
  assert_equal("k", lp.getItem(0).getItem)
  assert_equal("P", lp.getItem(1).getItem)
  assert_equal("q", lp.getItem(2).getItem)

  sorted = ChessDomain.new.normalise(lp)

  assert_equal(3, sorted.size)
  assert_equal("P", sorted.getItem(0).getItem)
  assert_equal("k", sorted.getItem(1).getItem)
  assert_equal("q", sorted.getItem(2).getItem)
end

unit_test "chess board and moves" do
  board1 = ChessDomain.constructBoard("......../......../......../....N.../......../......../......../........")
  board2 = ChessDomain.constructBoard("......../......../......../....N.../......p./......../......../........")
  board3 = ChessDomain.constructBoard("......../......../......../....N.../......P./......../......../........")
  board4 = ChessDomain.constructBoard("N......./......../......../......../......../......../......../........")
  board5 = ChessDomain.constructBoard("N...R.p./....P.../R...k.../.......R/......../......../......../........")

  assert_false(board1.isEmpty(3, 4))
  # check knight moves
  assert_equal(8, ChessDomain.new.proposeMovementFixations(board1, Square.new(3, 4)).size)
  assert_equal(8, ChessDomain.new.proposeMovementFixations(board2, Square.new(3, 4)).size)
  assert_equal(7, ChessDomain.new.proposeMovementFixations(board3, Square.new(3, 4)).size)
  assert_equal(2, ChessDomain.new.proposeMovementFixations(board4, Square.new(0, 0)).size)
  # check rook moves
  assert_equal(5, ChessDomain.new.proposeMovementFixations(board5, Square.new(0, 4)).size)
  assert_equal(10, ChessDomain.new.proposeMovementFixations(board5, Square.new(2, 0)).size)
  assert_equal(14, ChessDomain.new.proposeMovementFixations(board5, Square.new(3, 7)).size)
end
