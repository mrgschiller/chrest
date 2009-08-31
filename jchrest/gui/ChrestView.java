package jchrest.gui;

import jchrest.architecture.Chrest;

import java.awt.BorderLayout;
import java.awt.event.*;
import java.util.Observable;
import java.util.Observer;
import javax.swing.*;

public class ChrestView extends JFrame implements Observer {
  private Chrest _model;
  private ChrestLtmView _ltmView;
  private ChrestTimeView _timeView;

  public ChrestView (Chrest model) {
    super ("Chrest Model View");
    _model = model;
    _model.addObserver (this);
    _timeView = new ChrestTimeView (_model);
    _ltmView = new ChrestLtmView (_model);

    // catch close-window event
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent ev) { 
        closeView (); 
      }
    });
    createMenuBar ();

    setLayout (new BorderLayout ());
    add (_timeView, BorderLayout.NORTH);
    add (_ltmView, BorderLayout.CENTER);
    setSize (400, 300);
    setVisible (true);

    _ltmView.setStandardDisplay ();
  }

  private void createMenuBar () {
    JMenuBar mb = new JMenuBar ();
    mb.add (createViewMenu ());
    setJMenuBar (mb);
  }

  class CloseAction extends AbstractAction {
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
    menu.add (new CloseAction (this));

    return menu;
  }

  /** 
   * Implement the observable interface, and update the view whenever 
   * the underlying model has changed.
   */
  public void update(Observable o, Object arg) {
    _ltmView.update ();
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

