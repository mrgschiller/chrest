package jchrest.architecture;

import java.io.*;

import jchrest.lib.FileUtilities;
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

  /**
   * Write a description of the link to the given Writer object.
   */
  public void writeLink (Writer writer) throws IOException {
    FileUtilities.writeOpenTag (writer, "link");
    FileUtilities.writeOpenTag (writer, "test");
    _test.writePattern (writer);
    FileUtilities.writeCloseTag (writer, "test");

    FileUtilities.writeTaggedInt (writer, "child", _child.getReference ());

    FileUtilities.writeCloseTag (writer, "link");
  }
}

