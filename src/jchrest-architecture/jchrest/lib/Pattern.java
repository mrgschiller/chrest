// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.lib;

/** 
 * Parent class of all patterns.
 *
 * @author Peter C. R. Lane
 */
public abstract class Pattern {

  /**
   * Factory method to make a NumberPattern.
   */
  public static NumberPattern makeNumber (int number) {
    return NumberPattern.create (number);
  }

  /**
   * Factory method to make a StringPattern.
   */
  public static StringPattern makeString (String str) {
    return StringPattern.create (str);
  }

  private static ListPattern makeList (int[] numbers, Modality modality) {
    ListPattern list = new ListPattern (modality);
    for (int i = 0; i < numbers.length; ++i)
    {
      list.add (NumberPattern.create (numbers[i]));
    }
    return list;
  }

  /** Factory method to make a ListPattern given an array of numbers.
   * Each number is converted into a NumberPattern and added to the 
   * ListPattern.
   */
  public static ListPattern makeVisualList (int[] numbers) {
    return makeList (numbers, Modality.VISUAL);
  }

  public static ListPattern makeVerbalList (int[] numbers) {
    return makeList (numbers, Modality.VERBAL);
  }

  private static ListPattern makeList (String[] strings, Modality modality) {
    ListPattern list = new ListPattern (modality);
    for (int i = 0; i < strings.length; ++i)
    {
      list.add (StringPattern.create (strings[i]));
    }
    return list;
  }

  /** Factory method to make a ListPattern given an array of Strings.
   * Each number is converted into a StringPattern and added to the 
   * ListPattern.
   */
  public static ListPattern makeVisualList (String[] strings) {
    return makeList (strings, Modality.VISUAL);
  }

  public static ListPattern makeVerbalList (String[] strings) {
    return makeList (strings, Modality.VERBAL);
  }

  public static ListPattern makeActionList (String[] strings) {
    return makeList (strings, Modality.ACTION);
  }

  public abstract boolean matches (Pattern pattern);
  public abstract String toString ();
}

