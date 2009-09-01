package jchrest.gui;

import jchrest.architecture.*;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

public class ChrestStmView extends JPanel {
  private Chrest _model;
  private DefaultListModel _stmView;
  private JList _stmList;

  public ChrestStmView (Chrest model) {
    super ();

    _model = model;
    setBorder (new TitledBorder ("STM"));
    setLayout (new GridLayout (1, 1));

    _stmView = new DefaultListModel ();
    _stmList = new JList (_stmView);
    _stmList.setCellRenderer (new StmCellRendererer (_model));
    add (new JScrollPane (_stmList));
    update ();
  }

  public void update () {
    _stmView.clear ();
    for (Node node : _model.getVisualStm ().getContents ()) {
      _stmView.addElement (node);
    }
    _stmList.setModel (_stmView);
  }

  class StmCellRendererer extends JLabel implements ListCellRenderer {
    private Chrest _model;
    StmCellRendererer (Chrest model) {
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
      cell.setIcon (new NodeIcon ((Node)value));

      return cell;

        }
  }

  class NodeIcon implements Icon {
    private NodeDisplay _node;

    public NodeIcon (Node node) {
      _node = new NodeDisplay (node);
    }

    public void paintIcon (Component c, Graphics g, int x, int y) {
      _node.draw ((Graphics2D)g, x, y, getIconWidth(), getIconHeight(), Size.getValues().get (1));
    }

    public int getIconWidth  () { return _node.getWidth ( (Graphics2D)_stmList.getGraphics(), Size.getValues().get (1)); }
    public int getIconHeight () { return _node.getHeight( (Graphics2D)_stmList.getGraphics(), Size.getValues().get (1)); }
  }
}

