package jchrest.gui;

import jchrest.architecture.Chrest;
import jchrest.lib.ListPattern;
import jchrest.lib.Pattern;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;

/**
 * The main frame for the Chrest shell.
 *
 * @author Peter C. R. Lane
 */
public class Shell extends JFrame {
  private JFileChooser _fileChooser;
  private Chrest _model;

  private Shell () {
    super ("CHREST");

    _fileChooser = new JFileChooser ();
    _model = new Chrest ();

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    createMenuBar ();

    setContentPane( new JLabel ("Open some data to use shell") );

    setSize(600, 400);
    setLocationRelativeTo(null);
    setTheme ("Nimbus");
    setVisible (true);
  }

  public JFileChooser fileChooser () {
    return _fileChooser;
  }

  private void createMenuBar () {
    JMenuBar mb = new JMenuBar ();
    mb.add (createShellMenu ());
    mb.add (createDataMenu ());
    mb.add (createModelMenu ());
    setJMenuBar (mb);
  }

  /**
   * Simple action to display a message about this application.
   */
  class AboutAction extends AbstractAction implements ActionListener {
    private Shell _parent;

    AboutAction (Shell parent) {
      super ("About");
      _parent = parent;
    }

    public void actionPerformed (ActionEvent e) {
      JOptionPane.showMessageDialog (
          _parent, 
          "<HTML><P>This program is a graphical interface to the <BR>" +
          "Chrest cognitive architecture.  You can load data, train <BR>" +
          "models, and visualise results in a range of typical modelling <BR>" +
          "problems for Chrest.</P>" +
          "<P><P>Copyright (c) 2009, Peter C. R. Lane.</P></P>"+
          "<P>Released under GNU General Public License</a>, version 3.</P></HTML>",
          "About Chrest Shell", 
          JOptionPane.INFORMATION_MESSAGE);
    }
  }

  /**
   * Simple action to allow user to change look and feel.
   */
  class LookFeelAction extends AbstractAction implements ActionListener {
    private Shell _parent;

    LookFeelAction (Shell parent) {
      super ("Theme");
      _parent = parent;
    }

    public void actionPerformed (ActionEvent e) {
      Object [] possibleValues = new Object[UIManager.getInstalledLookAndFeels().length];
      int i = 0;
      for (UIManager.LookAndFeelInfo info : UIManager.getInstalledLookAndFeels ()) {
        possibleValues[i] = info.getName ();
        i += 1;
      }
      String theme = (String)JOptionPane.showInputDialog (_parent,
          "Select look and feel:",
          "Change theme",
          JOptionPane.INFORMATION_MESSAGE, 
          null,
          possibleValues,
          UIManager.getLookAndFeel().getName ());
      if (theme != null) {
        setTheme (theme);
      }
    }
  }

  /**
   * Action to load in a new data set from file.
   */
  class LoadDataAction extends AbstractAction implements ActionListener {
    private Shell _parent;

    LoadDataAction (Shell parent) {
      super ("Open");

      _parent = parent;
    }

    public void actionPerformed (ActionEvent e) {
      if (_parent.fileChooser().showOpenDialog (_parent) == JFileChooser.APPROVE_OPTION) {
        // -- following code for testing only
        List<ListPattern> items = new ArrayList<ListPattern> ();
        items.add (Pattern.makeList (new String[]{"A", "B", "C"}));
        items.add (Pattern.makeList (new String[]{"A", "D", "C"}));
        items.add (Pattern.makeList (new String[]{"C", "B", "D"}));

        _parent.setContentPane(new RecogniseAndLearnDemo (_model, items));
        _parent.validate ();
      }
    }
  }

  /**
   * Action to clear data held in the model.
   */
  class ClearModelAction extends AbstractAction implements ActionListener {
    private Chrest _model;
    private Shell _parent;

    ClearModelAction (Shell parent, Chrest model) {
      super ("Clear");

      _parent = parent;
      _model = model;
    }

    public void actionPerformed (ActionEvent e) {
      if (JOptionPane.OK_OPTION == JOptionPane.showConfirmDialog (
            _parent,
            "Are you sure you want to clear the model?",
            "Clear model?",
            JOptionPane.YES_NO_OPTION,
            JOptionPane.QUESTION_MESSAGE)) {
        _model.clear ();
      }
    }
  }

  /**
   * Action to show a dialog to change properties of model.
   */
  class ModelPropertiesAction extends AbstractAction implements ActionListener {
    private Chrest _model;

    ModelPropertiesAction (Chrest model) {
      super ("Properties");

      _model = model;
    }

    public void actionPerformed (ActionEvent e) {
    }
  }

  /**
   * Action to display information about the current model.
   */
  class ModelInformationAction extends AbstractAction implements ActionListener {
    private Chrest _model;

    ModelInformationAction (Chrest model) {
      super ("Information");

      _model = model;
    }

    public void actionPerformed (ActionEvent e) {
    }
  }

  /** 
   * Action to display a separate frame with information about the current model.
   * Frame is composed of separate views onto the model, all using the observer 
   * design pattern to keep updated as the model changes.
   */
  class ViewModelAction extends AbstractAction implements ActionListener {
    private Chrest _model;
    private Shell _parent;

    ViewModelAction (Shell parent, Chrest model) {
      super ("View");

      _parent = parent;
      _model = model;
    }

    public void actionPerformed (ActionEvent e) {
      new ChrestView (_model);
    }
  }

  private JMenu createShellMenu () {
    JMenuItem exit = new JMenuItem ("Exit");
    exit.addActionListener (new ActionListener () {
      public void actionPerformed (ActionEvent e) {
        System.exit (0);
      }
    });

    JMenu menu = new JMenu ("Shell");
    menu.add (new AboutAction (this));
    menu.add (new LookFeelAction (this));
    menu.add (new JSeparator ());
    menu.add (exit);

    return menu;
  }

  private JMenu createDataMenu () {
    JMenu menu = new JMenu ("Data");
    menu.add (new LoadDataAction (this));

    return menu;
  }

  private JMenu createModelMenu () {
    JMenu menu = new JMenu ("Model");
    menu.add (new ClearModelAction (this, _model));
    menu.add (new ModelPropertiesAction (_model));
    menu.add (new JSeparator ());
    menu.add (new ModelInformationAction (_model));
    menu.add (new ViewModelAction (this, _model));

    return menu;
  }

  /**
   * Set theme of user interface to the one named.
   */
  private void setTheme (String theme) {
    try { 
      for (UIManager.LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
        if (theme.equals(info.getName())) {
          UIManager.setLookAndFeel(info.getClassName());
          break;
        }
      }
    } catch (UnsupportedLookAndFeelException e) {
    } catch (ClassNotFoundException e) {
    } catch (InstantiationException e) {
    } catch (IllegalAccessException e) {
    }
    // make sure all components are updated
    SwingUtilities.updateComponentTreeUI(this);
    SwingUtilities.updateComponentTreeUI(_fileChooser);
  }

  /**
   * main method to get everything started.
   */
  public static void main (String[] args) {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      public void run() { new Shell (); }
    });
  }
}

