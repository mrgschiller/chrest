package jchrest.tests;

import org.junit.*;
import static org.junit.Assert.*;

import jchrest.architecture.*;
import jchrest.lib.*;

/**
 * Suite of tests to verify performance of the timinng elements of Chrest learning.
 * These tests are <em>Process Tests</em> as they directly 
 * test the performance of Chrest.
 *
 * @author Peter C. R. Lane
 */

public class TestChrestTimings {
  private Chrest _model;
  private ListPattern _patternA, _patternB;

  @Before public void setup () {
    _model = new Chrest ();
    _model.setRho (1.0f);
    _model.setFamiliarisationTime (2000);
    _model.setDiscriminationTime (10000);
    _patternA = Pattern.makeVisualList (new String[]{"B", "I", "F"});
    _patternB = Pattern.makeVisualList (new String[]{"X", "A", "Q"});
  }

  @Test public void testTimings () {
    assertEquals (0, _model.getClock ());
    // check change on one learning operation
    _model.recogniseAndLearn (_patternA);
    assertEquals (10000, _model.getClock ());
    // check changed on second learning operation
    _model.recogniseAndLearn (_patternA);
    assertEquals (20000, _model.getClock ());
    // check a busy model is not changed
    _model.recogniseAndLearn (_patternB, 10000);
    assertEquals (20000, _model.getClock ());
    _model.recogniseAndLearn (_patternA);
    assertEquals (30000, _model.getClock ());
    _model.recogniseAndLearn (_patternA);
    assertEquals (32000, _model.getClock ());
    _model.recogniseAndLearn (_patternA);
    assertEquals (34000, _model.getClock ());
    _model.recogniseAndLearn (_patternA);
    assertEquals (44000, _model.getClock ());
    _model.recogniseAndLearn (_patternA);
    assertEquals (46000, _model.getClock ());
  }
}

