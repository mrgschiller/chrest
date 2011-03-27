package jchrest.lib;

/**
 * Square is a convenience class to hold a row and column.
 *
 * @author Peter C. R. Lane
 */
public class Square {
  private int _column;
  private int _row;

  /**
   * Constructor makes an instance from given row and column.
   */
  public Square (int row, int column) {
    _column = column;
    _row = row;
  }

  /**
   * Accessor method for the column.
   */
  public int getColumn () {
    return _column;
  }

  /**
   * Accessor method for the row.
   */
  public int getRow () {
    return _row;
  }
}

