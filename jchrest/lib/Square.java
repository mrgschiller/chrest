package jchrest.lib;

/**
 * Square is a convenience class to hold a row and column.
 *
 * @author Peter C. R. Lane
 */
public class Square {
  private int _column;
  private int _row;

  public Square (int row, int column) {
    _column = column;
    _row = row;
  }

  public int getColumn () {
    return _column;
  }

  public int getRow () {
    return _row;
  }
}

