package jchrest.lib;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
  * The ChessDomain is used for chess modelling.
  */
public class ChessDomain implements DomainSpecifics {
  public ListPattern normalise (ListPattern pattern) {
    return pattern.sort (new Comparator<PrimitivePattern> () {
      public int compare (PrimitivePattern left, PrimitivePattern right) {
        assert (left instanceof ItemSquarePattern);
        assert (right instanceof ItemSquarePattern);
        ItemSquarePattern leftIos = (ItemSquarePattern)left;
        ItemSquarePattern rightIos = (ItemSquarePattern)right;

        // P p K k B b N n Q q R r - the canonical ordering of chess pieces, from deGroot and Gobet (1996)
        List<String> pieces = new ArrayList<String> ();
        pieces.add ("P"); pieces.add ("p");
        pieces.add ("K"); pieces.add ("k");
        pieces.add ("B"); pieces.add ("b");
        pieces.add ("N"); pieces.add ("n");
        pieces.add ("Q"); pieces.add ("q");
        pieces.add ("R"); pieces.add ("r");

        // check item
        if (pieces.indexOf (leftIos.getItem()) < pieces.indexOf (rightIos.getItem ())) return -1;
        if (pieces.indexOf (leftIos.getItem()) > pieces.indexOf (rightIos.getItem ())) return 1;
        // check column
        if (leftIos.getColumn () < rightIos.getColumn ()) return -1;
        if (leftIos.getColumn () > rightIos.getColumn ()) return 1;
        // check row
        if (leftIos.getRow () < rightIos.getRow ()) return -1;
        if (leftIos.getRow () > rightIos.getRow ()) return 1;
        return 0;
      }
    });
  }

  /**
   * A 'big piece' is anything other than a pawn.  
   * Used to indicate a salient piece for a novice chess player.
   */
  public static boolean isBigPiece (ItemSquarePattern ios) {
    return !(ios.getItem().equals ("P") || ios.getItem().equals ("p"));
  }

  /**
   * An 'offensive piece' is a piece on the other player's side.
   * e.g. a black piece on white's side of the board.
   * Used to indicate a salient piece for an inexperienced chess player.
   */
  public static boolean isOffensivePiece (ItemSquarePattern ios) {
    if (ios.getItem().isEmpty ()) return false;
    char piece = ios.getItem().charAt (0);
    if (Character.isLowerCase (piece) && ios.getRow () <= 4) return true; // black piece on white side
    if (Character.isUpperCase (piece) && ios.getRow () >= 5) return true; // white piece on black side
    return false;
  }

  public static ListPattern getSalientPieces (ListPattern pattern, int isExperienced) {
    return getSalientPieces (pattern, isExperienced == 1);
  }

  /**
   * Retrieve the salient pieces from the given pattern.
   * The 'isExperienced' flag is used to indicate level of skill of model:
   * isExperienced = false means use the 'isBigPiece' measure,
   * isExperienced = true means use the 'isOffensivePiece' measure.
   */
  public static ListPattern getSalientPieces (ListPattern pattern, Boolean isExperienced) {
    ListPattern result = new ListPattern (pattern.getModality ());
    for (int i = 0, n = pattern.size (); i < n; ++i) {
      if (pattern.getItem (i) instanceof ItemSquarePattern) {
        ItemSquarePattern ios = (ItemSquarePattern)(pattern.getItem (i));
        if (isExperienced && isOffensivePiece (ios)) {
          result.add (ios);
        } else if (!isExperienced && isBigPiece (ios)) {
          result.add (ios);
        } else {
          ; // leave out non-salient pieces
        }
      }
    }
    return result;
  }
}

