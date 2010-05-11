package jchrest.architecture;

import java.util.ArrayList;
import java.util.List;

public class Stm {
  private int _size;
  private List<Node> _items;

  public Stm (int size) {
    _size = size;
    _items = new ArrayList<Node> ();
  }

  public List<Node> getContents () {
    return _items;
  }

  public int getSize () {
    return _size;
  }

  public void setSize (int size) {
    _size = size;
    _items.clear ();
  }

  public int getCount () {
    return _items.size ();
  }

  public Node getItem (int index) {
    return _items.get (index);
  }

  /**
   * When adding a new node to STM, the most informative node is maintained 
   * at the top, as the hypothesis.
   */
  public void add (Node node) {
    Node hypothesis = node;
    for (Node check : _items) {
      if (check.information () > hypothesis.information ()) {
        hypothesis = check;
      }
    }
    _items.remove (node);
    _items.add (0, node);
    _items.remove (hypothesis);
    _items.add (0, hypothesis); // if node != hypothesis, then node will be 'below' hypothesis
    while (_items.size () > _size) {
      _items.remove (_items.size () - 1);
    }
  }

  /**
   * Replace the topmost (hypothesis) node with the given one.
   */
  public void replaceHypothesis (Node node) {
    if (_items.size () > 0) {
      _items.remove (0);
    }
    _items.add (0, node);
  }

  public void clear () {
    _items.clear ();
  }

  public void learnLateralLinks (Chrest model) {
    if (_items.size () >= 2) {
      _items.get(1).setFollowedBy (_items.get(0));
      model.advanceClock (model.getAddLinkTime ());
    }
  }
}


