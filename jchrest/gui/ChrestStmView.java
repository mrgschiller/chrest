package jchrest.gui;

import jchrest.architecture.*;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

public class ChrestStmView extends JPanel {
  private Chrest _model;
  private DefaultListModel _visualStmView, _verbalStmView;
  private JList _visualStmList, _verbalStmList;

  public ChrestStmView (Chrest model) {
    super ();

    _model = model;
    setLayout (new GridLayout (1, 1));

    JPanel visualPanel = new JPanel ();
    visualPanel.setLayout (new GridLayout (1, 1));
    visualPanel.setBorder (new TitledBorder ("Visual STM"));
    _visualStmView = new DefaultListModel ();
    _visualStmList = new JList (_visualStmView);
    _visualStmList.setCellRenderer (new StmCellRendererer (_model));
    visualPanel.add (new JScrollPane (_visualStmList));

    JPanel verbalPanel = new JPanel ();
    verbalPanel.setLayout (new GridLayout (1, 1));
    verbalPanel.setBorder (new TitledBorder ("Verbal STM"));
    _verbalStmView = new DefaultListModel ();
    _verbalStmList = new JList (_verbalStmView);
    _verbalStmList.setCellRenderer (new StmCellRendererer (_model));
    verbalPanel.add (new JScrollPane (_verbalStmList));
  
    JSplitPane jsp = new JSplitPane (JSplitPane.VERTICAL_SPLIT, visualPanel, verbalPanel);
    jsp.setOneTouchExpandable (true);
    add (jsp);

    update ();
  }

  public void update () {
    _visualStmView.clear ();
    for (Node node : _model.getVisualStm ().getContents ()) {
      _visualStmView.addElement (node);
    }
    _visualStmList.setModel (_visualStmView);

    _verbalStmView.clear ();
    for (Node node : _model.getVerbalStm ().getContents ()) {
      _verbalStmView.addElement (node);
    }
    _verbalStmList.setModel (_verbalStmView);
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
      cell.setIcon (new NodeIcon ((Node)value, list));

      return cell;

        }
  }

  class NodeIcon implements Icon {
    private NodeDisplay _node;
    private JList _stmList;

    public NodeIcon (Node node, JList stmList) {
      _node = new NodeDisplay (node);
      _stmList = stmList;
    }

    public void paintIcon (Component c, Graphics g, int x, int y) {
      _node.draw ((Graphics2D)g, x, y, getIconWidth(), getIconHeight(), Size.getValues().get (1));
    }

    public int getIconWidth  () { return _node.getWidth ( (Graphics2D)_stmList.getGraphics(), Size.getValues().get (1)); }
    public int getIconHeight () { return _node.getHeight( (Graphics2D)_stmList.getGraphics(), Size.getValues().get (1)); }
  }
}

