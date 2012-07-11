// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.lib;

import java.util.HashMap;
import java.util.Map;

/**
 * The NumberPattern is a type of PrimitivePattern used to hold 
 * numbers.  The number is treated as a single object.  Instances 
 * of this class are immutable.
 *
 * @author Peter C. R. Lane
 */
public class NumberPattern extends PrimitivePattern {

  /**
   * Static creator method attempts to retrieve a cached instance for given 
   * number, else creates and returns a new NumberPattern instance.
   */
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
  public boolean equalPrimitive (PrimitivePattern pattern) {
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

  /**
   * Return a string representation of this pattern.
   */
  public String toString () {
    return "" + _number;
  }

  // private fields
  private final int _number;
  private static final Map<Integer, NumberPattern> _cache = new HashMap<Integer, NumberPattern> ();
}

