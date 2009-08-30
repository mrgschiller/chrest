package jchrest.architecture;

import jchrest.lib.ListPattern;

public class Link {
  private ListPattern _test;
  private Node _child;

  public Link (ListPattern test, Node child) {
    _test = test;
    _child = child;
  }

  public Node getChildNode () {
    return _child;
  }

  public ListPattern getTest () {
    return _test;
  }

  public boolean passes (ListPattern pattern) {
    return _test.matches (pattern);
  }
}

