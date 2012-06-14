package jchrest.gui;

import jchrest.architecture.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

public class ListNodeRenderer extends JLabel implements ListCellRenderer {
  private Chrest _model;

  ListNodeRenderer (Chrest model) {
    _model = model;
  }

  public Component getListCellRendererComponent (
      JList list,
      Object value,
      int index,
      boolean isSelected,
      boolean cellHasFocus) {
    JLabel cell = new JLabel ("");
    cell.setBorder (new CompoundBorder (new EmptyBorder (3, 3, 3, 3), new EtchedBorder ()));
    cell.setIcon (new NodeIcon ((Node)value, list));

    return cell;

      }
}

