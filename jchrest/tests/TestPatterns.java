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
  private ItemSquarePattern _ios1, _ios1a, _ios2;
  private ListPattern _list1, _list2, _list3;

  @Before public void setupExamples () {
    _number1 = Pattern.makeNumber (1);
    _number2 = Pattern.makeNumber (2);
    _string1 = Pattern.makeString ("abc");
    _string2 = Pattern.makeString ("def");
    _ios1 = new ItemSquarePattern ("P", 2, 3);
    _ios1a = new ItemSquarePattern ("P", 2, 3);
    _ios2 = new ItemSquarePattern ("Q", 2, 3);
    _list1 = Pattern.makeVisualList (new int[]{1,2,3,4});
    _list2 = Pattern.makeVisualList (new String[]{"a", "b", "c", "d", "e"});
    _list3 = Pattern.makeVisualList(new int[]{1,2,3});
  }

  @Test public void testNumberMatches () {
    assertTrue (_number1.equalPrimitive (NumberPattern.create (1)));
    assertFalse (_number1.equalPrimitive (_number2));
    assertTrue (_number1.matches (NumberPattern.create (1)));
    assertFalse (_number1.matches (_number2));
  }

  @Test public void testStringMatches () {
    assertTrue (_string1.equalPrimitive (StringPattern.create ("abc")));
    assertFalse (_string1.equalPrimitive (_string2));
    assertTrue (_string1.matches (StringPattern.create ("abc")));
    assertFalse (_string1.matches (_string2));
  }

  @Test public void testIosMatches () {
    assertTrue (_ios1.equalPrimitive (_ios1a));
    assertFalse (_ios1.equalPrimitive (_ios2));
    assertTrue (_ios1.matches (_ios1a));
    assertFalse (_ios1.matches (_ios2));
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
    assertFalse (Pattern.makeVisualList(new int[]{}).equals (Pattern.makeVerbalList(new int[]{})));
    assertTrue (_list1.equals (Pattern.makeVisualList(new int[]{1,2,3,4})));
    assertFalse (_list1.equals (Pattern.makeVisualList(new int[]{1,2,3})));
    assertFalse (_list1.equals (Pattern.makeVisualList(new int[]{1,2,3,4,5})));
    assertFalse (_list1.equals (Pattern.makeVisualList(new int[]{1,2,4,5})));
    assertFalse (_list1.equals (_list2));
    ListPattern list1Copy = Pattern.makeVisualList(new int[]{1,2,3,4});
    list1Copy.setFinished ();
    assertFalse (_list1.equals (list1Copy));
    _list1.setFinished ();
    assertTrue (_list1.equals (list1Copy));
    // for ItemSquarePattern
    ListPattern iosList1 = new ListPattern ();
    iosList1.add (_ios1);
    ListPattern iosList2 = new ListPattern ();
    iosList2.add (_ios2);
    ListPattern iosList3 = new ListPattern ();
    iosList3.add (_ios1a);
    assertTrue (iosList1.equals (iosList3));
    assertFalse (iosList1.equals (iosList2));
  }

  @Test public void testListPatternMatches1 () {
    assertFalse (Pattern.makeVisualList(new int[]{}).matches (Pattern.makeVerbalList(new int[]{})));
    assertTrue ((new ListPattern ()).matches (_list1));
    assertTrue (_list1.matches (_list1));
    assertFalse (_list1.matches (_list2));
    assertTrue (_list3.matches(_list1));
    assertFalse (_list1.matches(_list3));
  }

  @Test public void testListPatternMatches2 () {
    ListPattern prim1 = Pattern.makeVisualList (new int[]{1});
    assertTrue (prim1.matches (_list1));
    ListPattern prim1Clone = prim1.clone ();
    prim1Clone.setFinished ();
    assertTrue (prim1.matches (prim1Clone));
    assertFalse (prim1Clone.matches (prim1));
  }
  
  @Test public void testListPatternMatches3 () {
    ListPattern empty = new ListPattern ();
    assertTrue (empty.matches (_list1));
    empty.setFinished ();
    assertFalse (empty.matches (_list1));
    assertTrue (empty.matches (empty));
  }

  @Test public void testListPatternAppend () {
    assertTrue (_list1.equals(_list3.append(Pattern.makeVisualList(new int[] {4}))));
    assertTrue (_list1.equals(_list3.append(Pattern.makeNumber(4))));
  }

  @Test public void testListPatternRemove () {
    assertTrue ((Pattern.makeVisualList(new int[]{4})).equals(_list1.remove(_list3)));
    assertTrue ((Pattern.makeVisualList(new int[]{4})).equals(_list1.remove(Pattern.makeVisualList(new int[]{1,2,3,5,6}))));
    ListPattern pattern = _list3.clone ();
    pattern.setFinished ();
    assertTrue (pattern.remove(_list3).isEmpty ());
    assertTrue (pattern.remove(_list3).isFinished ());
  }
}

