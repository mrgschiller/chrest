package jchrest.tests;

import org.junit.*;
import static org.junit.Assert.*;

import jchrest.lib.*;

/**
 * Tests to verify performance of chess domain class.
 *
 * @author Peter C. R. Lane
 */
public class TestChessDomain {
  @Test public void testPieceTypes () {
    ItemSquarePattern ios1 = new ItemSquarePattern ("P", 1, 1);
    ItemSquarePattern ios2 = new ItemSquarePattern ("Q", 2, 2);
    ItemSquarePattern ios3 = new ItemSquarePattern ("q", 2, 2);
    ItemSquarePattern ios4 = new ItemSquarePattern ("Q", 2, 7);
    ItemSquarePattern ios5 = new ItemSquarePattern ("q", 2, 7);

    assertFalse (ChessDomain.isBigPiece (ios1));
    assertTrue (ChessDomain.isBigPiece (ios2));

    assertFalse (ChessDomain.isOffensivePiece (ios1));
    assertFalse (ChessDomain.isOffensivePiece (ios2));
    assertTrue (ChessDomain.isOffensivePiece (ios3));
    assertTrue (ChessDomain.isOffensivePiece (ios4));
    assertFalse (ChessDomain.isOffensivePiece (ios5));

    ListPattern lp = new ListPattern ();
    for (ItemSquarePattern pat : (new ItemSquarePattern[] {ios1, ios2, ios3, ios4, ios5})) {
      lp.add (pat);
    }
    assertEquals (2, ChessDomain.getSalientPieces(lp, true).size ());
    assertEquals (4, ChessDomain.getSalientPieces(lp, false).size ());
  }

  @Test public void testChessBoard () {
    ChessDomain chessDomain = new ChessDomain ();

    Scene board1 = ChessDomain.constructBoard ("......../......../......../....N.../......../......../......../........");
    Scene board2 = ChessDomain.constructBoard ("......../......../......../....N.../......p./......../......../........");
    Scene board3 = ChessDomain.constructBoard ("......../......../......../....N.../......P./......../......../........");
    Scene board4 = ChessDomain.constructBoard ("N......./......../......../......../......../......../......../........");
    Scene board5 = ChessDomain.constructBoard ("N...R.p./....P.../R...k.../.......R/......../......../......../........");

    assertFalse (board1.isEmpty (3, 4));
    // check knight moves
    assertEquals (8, chessDomain.proposeMovementFixations(board1, new Square (3, 4)).size ());
    assertEquals (8, chessDomain.proposeMovementFixations(board2, new Square (3, 4)).size ());
    assertEquals (7, chessDomain.proposeMovementFixations(board3, new Square (3, 4)).size ());
    assertEquals (2, chessDomain.proposeMovementFixations(board4, new Square (0, 0)).size ());
    // check rook moves
    assertEquals (5, chessDomain.proposeMovementFixations(board5, new Square (0, 4)).size ());
    assertEquals (10, chessDomain.proposeMovementFixations(board5, new Square (2, 0)).size ());
    assertEquals (14, chessDomain.proposeMovementFixations(board5, new Square (3, 7)).size ());

  }
}
