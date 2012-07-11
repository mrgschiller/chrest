// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.lib;

/**
 * The ItemSquarePattern is a type of PrimitivePattern used to hold 
 * objects places on a square array.  The item-on-square is treated 
 * as a single object.  Instances of this class are immutable.
 *
 * @author Peter C. R. Lane
 */
public class ItemSquarePattern extends PrimitivePattern {

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
  public boolean equalPrimitive (PrimitivePattern pattern) {
    if (pattern instanceof ItemSquarePattern) {
      ItemSquarePattern ios = (ItemSquarePattern)pattern;
      return (_item.equals (ios.getItem ()) &&
          _column == ios.getColumn () &&
          _row == ios.getRow ());
    } else {
      return false;
    }
  }

  /** 
   * Two ItemSquarePatterns only match if they are the same.
   */
  public boolean matches (Pattern givenPattern) {
    if (!(givenPattern instanceof ItemSquarePattern)) return false;
    return this.equalPrimitive ((ItemSquarePattern)givenPattern);
  }

  /**
   * Return a string representation of the item on square.
   */
  public String toString () {
    return "[" + _item + " " + _column + " " + _row + "]";
  }

  // private fields
  private final String _item;
  private final int _column;
  private final int _row;
}

