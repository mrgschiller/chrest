package jchrest.gui;

import jchrest.architecture.*;
import java.awt.*;
import javax.swing.*;

public class NodeIcon implements Icon {
  private NodeDisplay _node;
  private Component _parent;
//  private JList _stmList

  public NodeIcon (Node node, Component parent) {
    _node = new NodeDisplay (node);
    _parent = parent;
//    _stmList = stmList;
  }

  public void paintIcon (Component c, Graphics g, int x, int y) {
    _node.draw ((Graphics2D)g, x, y, getIconWidth(), getIconHeight(), Size.getValues().get (1));
  }

  public int getIconWidth  () { return _node.getWidth ( (Graphics2D)_parent.getGraphics(), Size.getValues().get (1)); }
  public int getIconHeight () { return _node.getHeight( (Graphics2D)_parent.getGraphics(), Size.getValues().get (1)); }
}

