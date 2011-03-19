package jchrest.gui;

import jchrest.architecture.*;

import java.awt.*;
import java.awt.event.*;
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
    _visualStmList.setCellRenderer (new ListNodeRenderer (_model));
    _visualStmList.addMouseListener(new MouseAdapter() {
      public void mouseClicked(MouseEvent evt) {
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) { 
          int index = list.locationToIndex(evt.getPoint());
          new NodeView (_model, (Node)_visualStmView.getElementAt (index));
        }
      }
    });
    visualPanel.add (new JScrollPane (_visualStmList));

    JPanel verbalPanel = new JPanel ();
    verbalPanel.setLayout (new GridLayout (1, 1));
    verbalPanel.setBorder (new TitledBorder ("Verbal STM"));
    _verbalStmView = new DefaultListModel ();
    _verbalStmList = new JList (_verbalStmView);
    _verbalStmList.setCellRenderer (new ListNodeRenderer (_model));
    _verbalStmList.addMouseListener(new MouseAdapter() {
      public void mouseClicked(MouseEvent evt) {
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) { 
          int index = list.locationToIndex(evt.getPoint());
          new NodeView (_model, (Node)_verbalStmView.getElementAt (index));
        }
      }
    });
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
}

