// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.gui;

import jchrest.architecture.Chrest;
import jchrest.lib.FileUtilities;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.util.Observable;
import java.util.Observer;
import javax.swing.*;

public class ChrestView extends JFrame implements Observer {
  private Shell _shell;
  private Chrest _model;
  private ChrestLtmView _ltmView;
  private ChrestStmView _stmView;
  private ChrestTimeView _timeView;

  public ChrestView (Chrest model) {
    this (new Shell (), model);
  }

  public ChrestView (Shell shell, Chrest model) {
    super ("CHREST Model View");
    _shell = shell;
    _model = model;
    _model.addObserver (this);
    _timeView = new ChrestTimeView (_model);
    _ltmView = new ChrestLtmView (_model);
    _stmView = new ChrestStmView (_model);

    // catch close-window event
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent ev) { 
        closeView (); 
      }
    });
    createMenuBar ();

    // layout the components
    JPanel leftSide = new JPanel ();
    leftSide.setLayout (new BorderLayout ());
    leftSide.add (_timeView, BorderLayout.NORTH);
    leftSide.add (_stmView, BorderLayout.CENTER);
    JSplitPane jsp = new JSplitPane (JSplitPane.HORIZONTAL_SPLIT, leftSide, _ltmView);
    jsp.setOneTouchExpandable (true);

    setLayout (new GridLayout (1, 1));
    add (jsp);

    // finalise display settings
    setSize (550, 400);
    setVisible (true);
    // prompt the long-term memory to draw itself
    _ltmView.setStandardDisplay ();
  }

  private void createMenuBar () {
    JMenuBar mb = new JMenuBar ();
    mb.add (createViewMenu ());
    setJMenuBar (mb);
  }

  public void saveLongTermMemory (File file) {
    _ltmView.saveLongTermMemory (file);
  }

  class SaveLtmAction extends AbstractAction implements ActionListener {
    private ChrestView _parent;

    public SaveLtmAction (ChrestView parent) {
      super ("Save LTM", new ImageIcon (Shell.class.getResource ("icons/SaveAs16.gif")));

      _parent = parent;
    }

    public void actionPerformed (ActionEvent e) {
      File file = FileUtilities.getSaveFilename (_shell);
      if (file != null) {
        _parent.saveLongTermMemory (file);
      }
    }
  }

  class CloseAction extends AbstractAction implements ActionListener {
    private ChrestView _view;

    public CloseAction (ChrestView view) {
      super ("Close");
      _view = view;
    }

    public void actionPerformed (ActionEvent e) {
      _view.closeView ();
    }
  }

  private JMenu createViewMenu () {
    JMenu menu = new JMenu ("View");
    menu.setMnemonic (KeyEvent.VK_V);
    menu.add (new SaveLtmAction (this));
    menu.getItem(0).setMnemonic (KeyEvent.VK_S);
    menu.add (new CloseAction (this));
    menu.getItem(1).setMnemonic (KeyEvent.VK_C);

    return menu;
  }

  /** 
   * Implement the observable interface, and update the view whenever 
   * the underlying model has changed.
   */
  public void update(Observable o, Object arg) {
    _ltmView.update ();
    _stmView.update ();
    _timeView.update ();
  }

  /**
   * When closing the view, make sure the observer is detached from the model.
   */
  private void closeView () {
      _model.deleteObserver (this);
      setVisible (false);
      dispose ();
    }
}

