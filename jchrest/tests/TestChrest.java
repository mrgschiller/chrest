package jchrest.tests;

import org.junit.*;
import static org.junit.Assert.*;

import jchrest.architecture.*;
import jchrest.lib.*;

/**
 * Suite of tests to verify performance of the Chrest class.
 * These tests are <em>Process Tests</em> as they directly 
 * test the performance of Chrest.
 *
 * @author Peter C. R. Lane
 */
public class TestChrest {
  private Chrest _model1, _model2;
  private ListPattern _list1, _list2, _list3, _list3Test, _list4, _emptyList;
  private ListPattern _prim1, _prim2;
  private ListPattern _prim1Test;

  @Before public void setupModels () {
    _model1 = new Chrest ();
    _model2 = new Chrest ();

    _list1 = Pattern.makeList (new int[]{1,2,3,4});
    _list1.setFinished ();
    _list2 = Pattern.makeList (new int[]{3,4});
    _list2.setFinished ();
    _list3 = Pattern.makeList (new int[]{1,2});
    _list3.setFinished ();
    _list3Test = _list3.clone ();
    _list3Test.setNotFinished ();
    _list4 = Pattern.makeList (new int[]{1});
    _prim1 = Pattern.makeList (new int[]{1});
    _prim1.setFinished ();
    _prim1Test = _prim1.clone ();
    _prim1Test.setNotFinished ();
    _prim2 = Pattern.makeList (new int[]{2});
    _prim2.setFinished ();

    _emptyList = Pattern.makeList (new int[]{});
  }

  @Test public void baseCase () {
    assertTrue (_emptyList.equals (_model1.recognise (_emptyList).getImage ()));
  }

  @Test public void simpleLearning1 () {
    _model1.recogniseAndLearn (_list1);
    assertEquals (1, _model1.getLtm().getChildren().size ());
    Link firstChild = _model1.getLtm().getChildren().get(0);
    assertFalse (_emptyList.equals (firstChild.getChildNode().getContents ()));
    assertTrue (firstChild.getTest().equals (_prim1Test));
    assertTrue (firstChild.getChildNode().getContents().equals (_prim1Test));
    assertTrue (firstChild.getChildNode().getImage().equals (_prim1));
  }

  @Test public void simpleRetrieval1 () {
    _model1.recogniseAndLearn (_list1);
    Node node = _model1.recognise (_list1);
    assertFalse (_emptyList.equals (node.getContents ()));
    assertTrue (_prim1Test.equals (node.getContents ()));
    assertFalse (_emptyList.equals (node.getImage ()));
    assertTrue (_prim1.equals (node.getImage ()));
  }

  @Test public void simpleLearning2 () {
    _model1.recogniseAndLearn (_list1);
    _model1.recogniseAndLearn (_list1);
    assertEquals (2, _model1.getLtm().getChildren().size ());
    // check most recent becomes the first child node
    assertTrue (_prim2.equals (_model1.getLtm().getChildren().get(0).getChildNode().getImage ()));
    assertTrue (_prim1.equals (_model1.getLtm().getChildren().get(1).getChildNode().getImage ()));
    // discriminate from node 1
    Node node = _model1.getLtm().getChildren().get(1).getChildNode ();
    assertEquals (0, node.getChildren().size ());
    _model1.recogniseAndLearn (_list1);
    assertEquals (1, node.getChildren().size ());
    assertTrue (_emptyList.equals (node.getChildren().get(0).getChildNode().getImage ()));
    assertTrue (_list3Test.equals(node.getChildren().get(0).getChildNode().getContents ()));
    // now to familiarise
    Node node2 = node.getChildren().get(0).getChildNode ();
    _model1.recogniseAndLearn (_list1);
    assertTrue (_list4.equals (node2.getImage ()));
  }

  /**
   * Test that can learn a test link which is just 'pattern is finished', i.e. < $ >
   */
  @Test public void simpleLearning3 () {
    ListPattern list5 = Pattern.makeList (new String[] {"A", "B", "C"});
    ListPattern list6 = Pattern.makeList (new String[] {"A", "B"});
    list5.setFinished ();
    list6.setFinished ();
    for (int i = 0; i < 8; ++i) {
      _model1.recogniseAndLearn (list5);
    }
    assertTrue (list5.equals (_model1.recallPattern (list5)));
    assertTrue (list5.equals (_model1.recallPattern (list6)));
    Node node = _model1.recognise (list6);
    assertTrue (list5.equals (node.getImage ()));
    assertEquals (0, node.getChildren().size ());
    // learning should result in discrimination with < $ >
    _model1.recogniseAndLearn (list6);
    assertEquals (1, node.getChildren().size ());
  }

  /**
   * Test to see if model learns two lists completely.
   */
  @Test public void fullLearning () {
    for (int i = 0; i < 20; ++i) {
      _model1.recogniseAndLearn (_list2);
      _model1.recogniseAndLearn (_list3);
    }
    assertTrue (_list2.equals(_model1.recallPattern (_list2)));
    assertTrue (_list3.equals(_model1.recallPattern (_list3)));
  }
}

