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
  private ListPattern _iosList1, _emptyIosList, _iosPrim1, _iosPrim1Test;

  @Before public void setupModels () {
    _model1 = new Chrest ();
    _model2 = new Chrest ();

    // patterns for simple primitives

    _list1 = Pattern.makeVisualList (new int[]{1,2,3,4});
    _list1.setFinished ();
    _list2 = Pattern.makeVisualList (new int[]{3,4});
    _list2.setFinished ();
    _list3 = Pattern.makeVisualList (new int[]{1,2});
    _list3.setFinished ();
    _list3Test = _list3.clone ();
    _list3Test.setNotFinished ();
    _list4 = Pattern.makeVisualList (new int[]{1});
    _prim1 = Pattern.makeVisualList (new int[]{1});
    _prim1.setFinished ();
    _prim1Test = _prim1.clone ();
    _prim1Test.setNotFinished ();
    _prim2 = Pattern.makeVisualList (new int[]{2});
    _prim2.setFinished ();

    _emptyList = Pattern.makeVisualList (new int[]{});

    // patterns for ItemOnSquare

    _iosList1 = new ListPattern ();
    _iosList1.add (new ItemSquarePattern ("P", 1, 2));
    _iosList1.add (new ItemSquarePattern ("P", 2, 2));
    _iosList1.add (new ItemSquarePattern ("P", 3, 2));
    _iosList1.add (new ItemSquarePattern ("P", 4, 2));
    _emptyIosList = new ListPattern ();
    _iosPrim1 = new ListPattern ();
    _iosPrim1.add (new ItemSquarePattern ("P", 1, 2));
    _iosPrim1.setFinished ();
    _iosPrim1Test = _iosPrim1.clone ();
    _iosPrim1Test.setNotFinished ();
  }

  @Test public void baseCase () {
    assertTrue (Pattern.makeVisualList(new String[]{"Root"}).equals (_model1.recognise (_emptyList).getImage ()));
  }

  public void simpleLearning1 (ListPattern list, ListPattern emptyList, ListPattern prim, ListPattern primTest) {
    _model1.recogniseAndLearn (list);
    assertEquals (1, _model1.getLtmByModality(list).getChildren().size ());
    Link firstChild = _model1.getLtmByModality(list).getChildren().get(0);
    assertFalse (emptyList.equals (firstChild.getChildNode().getContents ()));
    assertTrue (firstChild.getTest().equals (primTest));
    assertTrue (firstChild.getChildNode().getContents().equals (primTest));
    assertTrue (firstChild.getChildNode().getImage().equals (prim));
  }

  @Test public void learningSimpleList1 () {
    simpleLearning1 (_list1, _emptyList, _prim1, _prim1Test);
  }

  @Test public void learningItemList1 () {
    simpleLearning1 (_iosList1, _emptyIosList, _iosPrim1, _iosPrim1Test);
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
    assertEquals (2, _model1.getLtmByModality(_list1).getChildren().size ());
    // check most recent becomes the first child node
    assertTrue (_prim2.equals (_model1.getLtmByModality(_list1).getChildren().get(0).getChildNode().getImage ()));
    assertTrue (_prim1.equals (_model1.getLtmByModality(_list1).getChildren().get(1).getChildNode().getImage ()));
    // discriminate from node 1
    Node node = _model1.getLtmByModality(_list1).getChildren().get(1).getChildNode ();
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
    ListPattern list5 = Pattern.makeVisualList (new String[] {"A", "B", "C"});
    ListPattern list6 = Pattern.makeVisualList (new String[] {"A", "B"});
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

