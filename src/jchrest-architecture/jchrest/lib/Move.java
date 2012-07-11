// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.lib;

/**
 * Hold the move of a piece to a position on a scene.
 *
 * @author Peter C. R. Lane
 */
public class Move {

  public Move (String piece, int row, int column) {
    _piece = piece;
    _row = row;
    _column = column;
  }

  public ListPattern asListPattern () {
    ListPattern result = new ListPattern (Modality.ACTION);
    result.add (new ItemSquarePattern (_piece, _column, _row));
    result.setFinished ();
    return result;
  }

  public String toString () { return _piece + " " + _row + " " + _column; }

  public String getPiece () { return _piece; }
  public int getRow () { return _row; }
  public int getColumn () { return _column; }

  private final String _piece;
  private final int _row;
  private final int _column;
}

