package jchrest.architecture;

import java.io.*;

import jchrest.lib.FileUtilities;
import jchrest.lib.ListPattern;

/**
 * Represents a test link within the model's long-term memory.
 * The link has a test, which must be passed when sorting a pattern through to the child node.
 */
public class Link {
  private ListPattern _test;
  private Node _child;

  public Link (ListPattern test, Node child) {
    _test = test;
    _child = child;
  }

  /**
   * Accessor to the link's child node.
   */
  public Node getChildNode () {
    return _child;
  }

  /**
   * Accessor to the link's test.
   */
  public ListPattern getTest () {
    return _test;
  }

  /**
   * Test if the given pattern can be sorted through this test link.
   * A test passes is the test matches the given pattern.
   */
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

