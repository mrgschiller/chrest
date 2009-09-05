package jchrest.tests;

import org.junit.*;
import static org.junit.Assert.*;

import jchrest.architecture.*;
import jchrest.lib.*;

/**
 * Suite of tests to verify performance of the Chrest STM class.
 * These tests are <em>Process Tests</em> as they directly 
 * test the performance of Chrest.
 *
 * @author Peter C. R. Lane
 */
public class TestStm {
  private Chrest _model;
  private ListPattern _patternA, _patternB, _patternC, _patternD;

  @Before public void setup () {
    _model = new Chrest ();
    _patternA = Pattern.makeVisualList (new String[]{"A", "E", "F"});
    _patternB = Pattern.makeVisualList (new String[]{"B", "E", "F"});
    _patternC = Pattern.makeVisualList (new String[]{"C", "E", "F"});
    _patternD = Pattern.makeVisualList (new String[]{"D", "E", "F"});
  }

  /**
   * First test, makes sure an empty LTM results in root node being 
   * placed in STM on trying to recognise a pattern.
   */
  @Test public void test1 () {
    assertEquals (0, _model.getVisualStm().getCount ());
    _model.recognise (_patternA);
    assertEquals (1, _model.getVisualStm().getCount ());
    assertTrue (_model.getLtmByModality (_patternA) == _model.getVisualStm().getItem (0));
  }

  /**
   * Second test, ensures that learning an item results in STM containing 
   * two nodes: the first is the new node, and the second is the root node.
   */
  @Test public void test2 () {
    _model.recogniseAndLearn (_patternA);
    assertEquals (2, _model.getVisualStm().getCount ());
    assertTrue (_model.getLtmByModality (_patternA) == _model.getVisualStm().getItem (1));
    assertEquals (1, _model.getVisualStm().getItem(0).getReference ());
  }

  /**
   * Third test, ensure that capacity of STM is respected.
   */
  @Test public void test3 () {
    _model.recogniseAndLearn (_patternA);
    assertEquals (2, _model.getVisualStm().getCount ());
    _model.recogniseAndLearn (_patternB);
    assertEquals (3, _model.getVisualStm().getCount ());
    _model.recogniseAndLearn (_patternC);
    assertEquals (4, _model.getVisualStm().getCount ());
    _model.recogniseAndLearn (_patternD);
    assertEquals (4, _model.getVisualStm().getCount ());
    _model.recogniseAndLearn (_patternA);
    assertEquals (4, _model.getVisualStm().getCount ());
  }

  /**
   * Fourth test, ensure that a new item, if repeated, is correctly placed 
   * into STM.
   */
  @Test public void test4 () {
    _model.recogniseAndLearn (_patternA);
    Node node = _model.getVisualStm().getItem (0);
    _model.recogniseAndLearn (_patternB);
    _model.recogniseAndLearn (_patternC);
    // check that node occurs only once in the STM, and that at the fourth place down
    assertEquals (3, _model.getVisualStm().getContents().indexOf (node));
    assertEquals (3, _model.getVisualStm().getContents().lastIndexOf (node));
    // check that newly recognised node occurs only once in the STM, and 
    // that at the first place down
    _model.recognise (_patternA);
    assertEquals (0, _model.getVisualStm().getContents().indexOf (node));
    assertEquals (0, _model.getVisualStm().getContents().lastIndexOf (node));
  }
}

