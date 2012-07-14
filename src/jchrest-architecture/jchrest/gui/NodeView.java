// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.gui;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import jchrest.architecture.Chrest;
import jchrest.architecture.Link;
import jchrest.architecture.Node;

public class NodeView extends JFrame implements java.util.Observer {
  private final Chrest _model;
  private final Node _node;
  private final JLabel _contentsLabel;
  private final JLabel _imageLabel;
  private final JLabel _followedBy;
  private final JLabel _namedBy;
  private final DefaultListModel _childLinksView, _similarityLinksView;
  private final JList _childLinks, _similarityLinks;

  public NodeView (Chrest model, Node node) {
    _model = model;
    _node = node;
    _node.addObserver (this);
    setTitle ("Node: " + _node.getReference ());
    addWindowListener (new WindowAdapter () {
      public void windowClosing (WindowEvent e) {
        ((NodeView)e.getWindow()).close ();
      }
    });

    // create and add the widgets
    _contentsLabel = new JLabel (_node.getContents().toString ());
    _imageLabel = new JLabel (_node.getImage().toString ());

    JPanel fields = new JPanel ();
    fields.setLayout (new GridLayout (4, 2));
    fields.add (new JLabel ("Contents: ", SwingConstants.RIGHT));
    fields.add (_contentsLabel);
    fields.add (new JLabel ("Image: ", SwingConstants.RIGHT));
    fields.add (_imageLabel);
    
    _followedBy = new JLabel ("");
    _followedBy.setBorder (new CompoundBorder (new EmptyBorder (3, 3, 3, 3), new EtchedBorder ()));
    if (_node.getFollowedBy () != null) {
      _followedBy.setIcon (new NodeIcon (_node.getFollowedBy (), _followedBy));
    }
    fields.add (new JLabel ("Followed by: ", SwingConstants.RIGHT));
    fields.add (_followedBy);

    _namedBy = new JLabel ("");
    _namedBy.setBorder (new CompoundBorder (new EmptyBorder (3, 3, 3, 3), new EtchedBorder ()));
    if (_node.getNamedBy () != null) {
      _namedBy.setIcon (new NodeIcon (_node.getNamedBy (), _namedBy));
    }
    fields.add (new JLabel ("Named by: ", SwingConstants.RIGHT));
    fields.add (_namedBy);

    _childLinksView = new DefaultListModel ();
    _childLinks = new JList (_childLinksView);
    _childLinks.setCellRenderer (new ListNodeRenderer (_model));
    _childLinks.setLayoutOrientation (JList.HORIZONTAL_WRAP);
    _childLinks.setVisibleRowCount (1);
    _childLinks.addMouseListener(new MouseAdapter () {
      public void mouseClicked(MouseEvent evt) {
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) { 
          int index = list.locationToIndex(evt.getPoint());
          new NodeView (_model, (Node)_childLinksView.getElementAt (index));
        }
      }
    });

    _similarityLinksView = new DefaultListModel ();
    _similarityLinks = new JList (_childLinksView);
    _similarityLinks.setCellRenderer (new ListNodeRenderer (_model));
    _similarityLinks.setLayoutOrientation (JList.HORIZONTAL_WRAP);
    _childLinks.setVisibleRowCount (1);
    _similarityLinks.addMouseListener(new MouseAdapter () {
      public void mouseClicked(MouseEvent evt) {
        JList list = (JList)evt.getSource();
        if (evt.getClickCount() == 2) { 
          int index = list.locationToIndex(evt.getPoint());
          new NodeView (_model, (Node)_similarityLinksView.getElementAt (index));
        }
      }
    });

    setLayout (new BorderLayout ());
    add (fields, BorderLayout.NORTH);
    add (new JScrollPane (_childLinks));
    add (new JScrollPane (_similarityLinks), BorderLayout.SOUTH);

    pack ();
    setVisible (true);

    updateDisplays ();
  }

  void close () {
    _node.deleteObserver (this);
    if (_node.getFollowedBy () != null) {
      _node.getFollowedBy().deleteObserver (this);
    }
    if (_node.getNamedBy () != null) {
      _node.getNamedBy().deleteObserver (this);
    }
    for (Link link : _node.getChildren ()) {
      link.getChildNode().deleteObserver (this);
    }
    for (Node node : _node.getSemanticLinks ()) {
      node.deleteObserver (this);
    }
    setVisible (false);
    dispose ();
  }

  private void updateDisplays () {
    _imageLabel.setText (_node.getImage().toString ());
    if (_node.getFollowedBy () != null) {
      _followedBy.setIcon (new NodeIcon (_node.getFollowedBy (), _followedBy));
      _node.getFollowedBy().addObserver (this);
    }
    if (_node.getNamedBy () != null) {
      _namedBy.setIcon (new NodeIcon (_node.getNamedBy (), _namedBy));
      _node.getNamedBy().addObserver (this);
    }

    _childLinksView.clear ();
    for (Link link: _node.getChildren ()) {
      _childLinksView.addElement (link.getChildNode ());
      link.getChildNode().addObserver (this);
    }
    _childLinks.setModel (_childLinksView);

    _similarityLinksView.clear ();
    for (Node node : _node.getSemanticLinks ()) {
      _similarityLinksView.addElement (node);
      node.addObserver (this);
    }
    _similarityLinks.setModel (_similarityLinksView);
  }

  public void update (java.util.Observable o, Object arg) {
    if (arg instanceof String && ((String)arg).equals("close")) {
      close ();
    } else {
      // update displays
      updateDisplays ();
    }
  }
}

