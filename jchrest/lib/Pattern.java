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

  /** Factory method to make a ListPattern given an array of numbers.
   * Each number is converted into a NumberPattern and added to the 
   * ListPattern.
   */
  public static ListPattern makeList (int[] numbers) {
    ListPattern list = new ListPattern ();
    for (int i = 0; i < numbers.length; ++i)
    {
      list.add (NumberPattern.create (numbers[i]));
    }
    return list;
  }

  /** Factory method to make a ListPattern given an array of Strings.
   * Each number is converted into a StringPattern and added to the 
   * ListPattern.
   */
  public static ListPattern makeList (String[] strings) {
    ListPattern list = new ListPattern ();
    for (int i = 0; i < strings.length; ++i)
    {
      list.add (StringPattern.create (strings[i]));
    }
    return list;
  }

  public abstract boolean equals (Pattern pattern);
  public abstract boolean matches (Pattern pattern);
  public abstract String toString ();
}

