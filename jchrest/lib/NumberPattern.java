package jchrest.lib;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

/**
 * The NumberPattern is a type of PrimitivePattern used to hold 
 * numbers.  The number is treated as a single object.
 *
 * @author Peter C. R. Lane
 */
public class NumberPattern extends PrimitivePattern {
  private int _number;
  private static final Map<Integer, NumberPattern> _cache = new HashMap<Integer, NumberPattern> ();

  public static NumberPattern create (int num) {
    Integer number = new Integer (num);
    if (!_cache.containsKey (number)) {
      _cache.put (number, new NumberPattern (num));
    }
    return _cache.get (number);
  }

  /** 
   * Constructor takes an int to define the contents of this pattern.
   */
  private NumberPattern (int number) {
    _number = number;
  }

  /** 
   * Accessor method for the stored number.
   */
  public int getNumber () {
    return _number;
  }

  /**
   * Two NumberPatterns are only equal if their stored numbers are the same.
   */
  public boolean equals (Pattern pattern) {
    if (pattern instanceof NumberPattern) {
      return _number == ((NumberPattern)pattern).getNumber ();
    } else {
      return false;
    }
  }

  /**
   * Two NumberPatterns only match if their stored numbers are the same.
   */
  public boolean matches (Pattern pattern) {
    if (pattern instanceof NumberPattern) {
      return _number == ((NumberPattern)pattern).getNumber ();
    } else {
      return false;
    }
  }

  public String toString () {
    return "" + _number;
  }

  public void writePattern (Writer writer) throws IOException {
    FileUtilities.writeTaggedInt (writer, "number-pattern", _number);
  }
}

