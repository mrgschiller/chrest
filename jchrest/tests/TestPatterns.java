package jchrest.tests;

import org.junit.*;
import static org.junit.Assert.*;

import jchrest.lib.*;

/**
 * Suite of tests to verify performance of the Pattern classes.
 * These tests are <em>Process Tests</em> as the performance of 
 * Chrest is dependent on the properties of its basic data types.
 *
 * @author Peter C. R. Lane
 */
public class TestPatterns {
  private NumberPattern _number1, _number2;
  private StringPattern _string1, _string2;
  private ListPattern _list1, _list2, _list3;

  @Before public void setupExamples () {
    _number1 = Pattern.makeNumber (1);
    _number2 = Pattern.makeNumber (2);
    _string1 = Pattern.makeString ("abc");
    _string2 = Pattern.makeString ("def");
    _list1 = Pattern.makeList (new int[]{1,2,3,4});
    _list2 = Pattern.makeList (new String[]{"a", "b", "c", "d", "e"});
    _list3 = Pattern.makeList(new int[]{1,2,3});
  }

  @Test public void testNumberMatches () {
    assertTrue (_number1.equals (new NumberPattern (1)));
    assertFalse (_number1.equals (_number2));
    assertTrue (_number1.matches (new NumberPattern (1)));
    assertFalse (_number1.matches (_number2));
  }

  @Test public void testStringMatches () {
    assertTrue (_string1.equals (new StringPattern ("abc")));
    assertFalse (_string1.equals (_string2));
    assertTrue (_string1.matches (new StringPattern ("abc")));
    assertFalse (_string1.matches (_string2));
  }

  @Test public void testMixedMatches () {
    assertFalse (_number1.equals (_string1));
    assertFalse (_number1.matches (_string1));
    assertFalse (_string1.equals (_number1));
    assertFalse (_string1.matches (_number1));
  }

  @Test public void testListPatternFinished () {
    assertFalse (_list1.isFinished ());
    _list1.setFinished ();
    assertTrue (_list1.isFinished ());
  }

  @Test public void testListPatternEquality () {
    assertTrue (_list1.equals (Pattern.makeList(new int[]{1,2,3,4})));
    assertFalse (_list1.equals (Pattern.makeList(new int[]{1,2,3})));
    assertFalse (_list1.equals (Pattern.makeList(new int[]{1,2,3,4,5})));
    assertFalse (_list1.equals (Pattern.makeList(new int[]{1,2,4,5})));
    assertFalse (_list1.equals (_list2));
    ListPattern list1Copy = Pattern.makeList(new int[]{1,2,3,4});
    list1Copy.setFinished ();
    assertFalse (_list1.equals (list1Copy));
    _list1.setFinished ();
    assertTrue (_list1.equals (list1Copy));
  }

  @Test public void testListPatternMatches () {
    assertTrue ((new ListPattern ()).matches (_list1));
    assertTrue (_list1.matches (_list1));
    assertFalse (_list1.matches (_list2));
    assertTrue (_list3.matches(_list1));
    assertFalse (_list1.matches(_list3));
    ListPattern prim1 = Pattern.makeList (new int[]{1});
    prim1.setFinished ();
    assertTrue (prim1.matches (_list1));
  }

  @Test public void testListPatternAppend () {
    assertTrue (_list1.equals(_list3.append(Pattern.makeList(new int[] {4}))));
    assertTrue (_list1.equals(_list3.append(Pattern.makeNumber(4))));
  }

  @Test public void testListPatternRemove () {
    assertTrue ((Pattern.makeList(new int[]{4})).equals(_list1.remove(_list3)));
    assertTrue ((Pattern.makeList(new int[]{4})).equals(_list1.remove(Pattern.makeList(new int[]{1,2,3,5,6}))));
  }
}

