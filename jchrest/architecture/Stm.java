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

  public int getSize () {
    return _size;
  }

  public void setSize (int size) {
    _size = size;
    _items.clear ();
  }

  public void add (Node node) {
    _items.add (0, node);
    while (_items.size () > _size) {
      _items.remove (_items.size () - 1);
    }
  }

  public void learnLateralLinks (Chrest model) {
    if (_items.size () >= 2) {
      _items.get(1).setFollowedBy (_items.get(0));
      model.advanceClock (model.getAddLinkTime ());
    }
  }
}


