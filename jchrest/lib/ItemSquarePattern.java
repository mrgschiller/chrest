package jchrest.lib;

/**
 * The ItemSquarePattern is a type of PrimitivePattern used to hold 
 * objects places on a square array.  The item-on-square is treated 
 * as a single object.
 *
 * @author Peter C. R. Lane
 */
public class ItemSquarePattern extends PrimitivePattern {
  private String _item;
  private int _column;
  private int _row;

  /**
   * Constructor takes a string to identify the item, and a column and row
   * to identify the square.
   */
  public ItemSquarePattern (String item, int column, int row) {
    _item = item;
    _column = column;
    _row = row;
  }

  /** 
   * Accessor method for the stored item.
   */
  public String getItem () {
    return _item;
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

  /**
   * Two ItemSquarePatterns are only equal if all their parts are the same.
   */
  public boolean equals (Pattern givenPattern) {
    if (givenPattern instanceof ItemSquarePattern) {
      ItemSquarePattern pattern = (ItemSquarePattern)givenPattern;
      return (_item.equals (pattern.getItem ()) &&
          _column == pattern.getColumn () &&
          _row == pattern.getRow ());
    } else {
      return false;
    }
  }

  /** 
   * Two ItemSquarePatterns only match if they are the same.
   */
  public boolean matches (Pattern givenPattern) {
    return this.equals (givenPattern);
  }

  public String toString () {
    return "[" + _item + " " + _column + " " + _row + "]";
  }
}


