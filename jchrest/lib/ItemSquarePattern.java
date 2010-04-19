package jchrest.lib;

import java.io.*;

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

  public String toString () {
    return "[" + _item + " " + _column + " " + _row + "]";
  }

  public void writePattern (Writer writer) throws IOException {
    FileUtilities.writeOpenTag (writer, "item-on-square-pattern");
    FileUtilities.writeTaggedString (writer, "item", _item);
    FileUtilities.writeTaggedInt (writer, "column", _column);
    FileUtilities.writeTaggedInt (writer, "row", _row);
    FileUtilities.writeCloseTag (writer, "item-on-square-pattern");
  }

  /**
   * Read an item-on-square pattern from given reader object.  Assumes that column/row cannot be -1.
   */
  public static ItemSquarePattern readPattern (BufferedReader reader) throws ParsingErrorException {
    int column = -1;
    int row = -1; 
    String item = null;

    FileUtilities.acceptOpenTag (reader, "item-on-square-pattern");
    while (!FileUtilities.checkCloseTag (reader, "item-on-square-pattern")) {
      if (FileUtilities.checkOpenTag (reader, "item")) {
        item = FileUtilities.readStringInTag (reader, "item");
      } else if (FileUtilities.checkOpenTag (reader, "column")) {
        column = FileUtilities.readIntInTag (reader, "column");
      } else if (FileUtilities.checkOpenTag (reader, "row")) {
        row = FileUtilities.readIntInTag (reader, "row");
      } else { // unknown tag
        throw new ParsingErrorException ();
      }
    }

    FileUtilities.acceptCloseTag (reader, "item-on-square-pattern");

    if (item == null || column == -1 || row == -1) {
      throw new ParsingErrorException ();
    }
    return new ItemSquarePattern (item, column, row);
  }
}


