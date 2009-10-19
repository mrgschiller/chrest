package jchrest.gui;

import jchrest.architecture.*;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;

/**
 * Display a node of the discrimination network.
 *
 * @author Peter C. R. Lane
 */
class NodeDisplay implements LtmTreeViewNode {
  private Node _node;
  private List<LtmTreeViewNode> _children;

  public NodeDisplay (Node node) {
    _node = node;
    _children = new ArrayList<LtmTreeViewNode> ();
  }

  private final int ROOTNODE_SIZE = 11;

  public List<LtmTreeViewNode> getChildren () {
    return _children;
  }

  private String toDisplay () {
    if (_node.getReference () == 0) {
      return _node.getImage().getModalityString ();
    } else {
      return _node.getImage().toString ();
    }
  }

  public int getWidth (Graphics2D g, Size size) {
    int width =  2 * size.getMargin ();

    if (isRoot ()) {
      width += ROOTNODE_SIZE;
    } else if ( size.isSmall () ) {
      width = size.getSmallSize ();
    } else {
      int fixedItemsWidth = Math.max (getNodeNumberWidth (g, size), 
          size.getWidth (toDisplay (), g));
      if (_node.getFollowedBy () != null) {
        fixedItemsWidth = Math.max (fixedItemsWidth, 
            size.getWidth (_node.getFollowedBy().getReference() + "", g));
      }
      if (_node.getNamedBy () != null) {
        fixedItemsWidth = Math.max (fixedItemsWidth, 
            size.getWidth (_node.getNamedBy().getReference() + "", g));
      }
      width += fixedItemsWidth;
    }

    return width;
  }

  public int getHeight (Graphics2D g, Size size) {
    int height = 2 * size.getMargin ();

    if (isRoot ()) {
      height += ROOTNODE_SIZE;
    } else if ( size.isSmall () ) {
      height = size.getSmallSize ();
    } else {
      height += getNodeNumberHeight (g, size);
      height += size.getMargin (); // gap between the two
      height += size.getHeight (toDisplay ().toString (), g);
      if (_node.getFollowedBy() != null) {
        height += size.getMargin ();
        height += size.getHeight (_node.getFollowedBy().getReference() + "", g);
      }
      if (_node.getNamedBy() != null) {
        height += size.getMargin ();
        height += size.getHeight (_node.getNamedBy().getReference() + "", g);
      }
    }
    return height;
  }

  public void draw (Graphics2D g, int x, int y, int w, int h, Size size) {
    if (isRoot ()) {
      drawRootNode (g, x, y, size);
    } else if ( size.isSmall () ) {
      drawSmallNode (g, x, y, w, h);
    } else {
      drawRegularNode (g, x, y, w, h, size);
    }
  }

  public boolean isRoot () {
    return _node == null;
  }

  public void add (LtmTreeViewNode node) {
    _children.add (node);
  }

  private void drawRootNode (Graphics2D g, int x, int y, Size size) {
    int m = size.getMargin ();
    g.setColor (Color.BLACK);
    g.fillOval (x+m, y+m, ROOTNODE_SIZE, ROOTNODE_SIZE);
  }

  private void drawSmallNode (Graphics2D g, int x, int y, int w, int h) {
    g.setColor (Color.BLUE);
    g.fillRect (x, y, w, h);
  }

  private void drawRegularNode (Graphics2D g, int x, int y, int w, int h, Size size) {
    g.setBackground (Color.WHITE);
    g.clearRect (x+1, y+1, w-1, h-1);
    g.setColor (Color.BLACK);

    size.drawText (g, x, y, getNodeNumberString ());

    int textHeight = size.getHeight (getNodeNumberString (), g);
    size.drawText (g, x, y + textHeight + size.getMargin (), toDisplay ().toString ());
    if (_node.getFollowedBy () != null) {
      textHeight += size.getMargin () + size.getHeight ((_node.getFollowedBy().getReference() + ""), g);
      g.setColor (Color.BLUE);
      size.drawText (g, x, y + textHeight + size.getMargin (), _node.getFollowedBy().getReference() + "");
    }
    if (_node.getNamedBy () != null) {
      textHeight += size.getMargin () + size.getHeight ((_node.getNamedBy().getReference() + ""), g);
      g.setColor (Color.GREEN);
      size.drawText (g, x, y + textHeight + size.getMargin (), _node.getNamedBy().getReference() + "");
    }
  }

  private String getNodeNumberString () { 
    return "Node: " + _node.getReference ();
  }

  private int getNodeNumberWidth (Graphics2D g, Size size) {
    return (int)(size.getTextBounds(getNodeNumberString (), g).getWidth ());
  }

  private int getNodeNumberHeight (Graphics2D g, Size size) {
    return (int)(size.getTextBounds(getNodeNumberString (), g).getHeight ());
  }
}


