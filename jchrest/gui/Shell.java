package jchrest.gui;

import jchrest.architecture.Chrest;
import jchrest.lib.ListPattern;
import jchrest.lib.PairedPattern;
import jchrest.lib.Pattern;
import jchrest.lib.Scenes;

import java.awt.GridLayout;
import java.awt.event.*;
import java.io.*;
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
    setLocationRelativeTo (null);
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
          "Chrest cognitive architecture.  You can load <BR>" +
          "data, train models, and visualise results in a <BR>" +
          "range of typical modelling problems for Chrest.</P>" +
          "<P><P>Copyright (c) 2010, Peter C. R. Lane.</P></P>" +
          "<P>Released under GNU General Public License</a>, version 3.</P>" + 
          
          "<p>See <a href=\"http://chrest.info\">http://chrest.info</a> for more information.</P></HTML>",
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

    private List<ListPattern> readItems (BufferedReader input) throws IOException {
      List<ListPattern> items = new ArrayList<ListPattern> ();
      String line = input.readLine ();

      while (line != null) {
        ListPattern pattern = Pattern.makeVisualList (line.trim().split("[, ]"));
        pattern.setFinished ();
        items.add (pattern);
        line = input.readLine ();
      } 

      return items;
    }

    private List<PairedPattern> readPairedItems (BufferedReader input, boolean secondVerbal) throws IOException {
      List<PairedPattern> items = new ArrayList<PairedPattern> ();
      String line = input.readLine ();
      while (line != null) {
        String[] pair = line.split (":");
        if (pair.length != 2) throw new IOException (); // malformed pair
        ListPattern pat1 = Pattern.makeVisualList (pair[0].trim().split("[, ]"));
        pat1.setFinished ();
        ListPattern pat2;
        if (secondVerbal) {
          pat2 = Pattern.makeVerbalList (pair[1].trim().split("[, ]"));
        } else {
          pat2 = Pattern.makeVisualList (pair[1].trim().split("[, ]"));
        }
        pat2.setFinished ();
        items.add (new PairedPattern (pat1, pat2));

        line = input.readLine ();
      }

      return items;
    }

    public void actionPerformed (ActionEvent e) {
      if (_parent.fileChooser().showOpenDialog (_parent) == JFileChooser.APPROVE_OPTION) {
        if (_parent.fileChooser().getSelectedFile().exists ()) {
          try {
            String task = "";
            BufferedReader input = new BufferedReader (new FileReader (_parent.fileChooser().getSelectedFile ()));
            String line = input.readLine ();
            if (line != null) {
              task = line.trim ();
            }              

            if (task.equals ("recognise-and-learn")) {
              _parent.setContentPane (new RecogniseAndLearnDemo (_model, readItems (input)));
              _parent.validate ();
            } else if (task.equals ("serial-anticipation")) {
              _parent.setContentPane (new SerialAnticipationExperiment (_model, readItems (input)));
              _parent.validate ();
            } else if (task.equals ("paired-associate")) {
              _parent.setContentPane (new PairedAssociateExperiment (_model, readPairedItems (input, false)));
              _parent.validate ();
            } else if (task.equals ("categorisation")) {
              _parent.setContentPane (new CategorisationExperiment (_model, readPairedItems (input, true)));
              _parent.validate ();
            } else if (task.equals ("visual-search")) {
              Scenes scenes = Scenes.read (input); // throws IOException if any problem
              _parent.setContentPane (new VisualSearchPane (_model, scenes));
              _parent.validate ();
            } else {
              JOptionPane.showMessageDialog (_parent,
                  "Invalid task on first line of file",
                  "File error",
                  JOptionPane.ERROR_MESSAGE);
            }
          } catch (IOException ioe) {
            JOptionPane.showMessageDialog (_parent, 
                "There was an error in processing your file", 
                "File error",
                JOptionPane.ERROR_MESSAGE);
          }
        }
      }
    }
  }

  /**
   * Action to clear data held in the model.
   */
  class ClearModelAction extends AbstractAction implements ActionListener {
    private Shell _parent;

    ClearModelAction (Shell parent) {
      super ("Clear");

      _parent = parent;
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
    private Shell _parent;

    ModelPropertiesAction (Shell parent) {
      super ("Properties");

      _parent = parent;
    }

    public void actionPerformed (ActionEvent e) {
      if (JOptionPane.OK_OPTION == JOptionPane.showOptionDialog (_parent, 
            properties(), 
            "Chrest: Model properties", 
            JOptionPane.OK_CANCEL_OPTION,
            JOptionPane.PLAIN_MESSAGE,
            null, null, 0)) {
        _model.setAddLinkTime (((SpinnerNumberModel)_addLinkTime.getModel()).getNumber().intValue ());
        _model.setDiscriminationTime (((SpinnerNumberModel)_discriminationTime.getModel()).getNumber().intValue ());
        _model.setFamiliarisationTime (((SpinnerNumberModel)_familiarisationTime.getModel()).getNumber().intValue ());
        _model.setRho (((SpinnerNumberModel)_rhoEntry.getModel()).getNumber().floatValue ());
        _model.setVisualStmSize (((SpinnerNumberModel)_visualStmSize.getModel()).getNumber().intValue ());
        _model.setVerbalStmSize (((SpinnerNumberModel)_verbalStmSize.getModel()).getNumber().intValue ());
        _model.getPerceiver().setFieldOfView (((SpinnerNumberModel)_fieldOfView.getModel()).getNumber().intValue ());
      }
    }

    private JSpinner _addLinkTime;
    private JSpinner _discriminationTime;
    private JSpinner _familiarisationTime;
    private JSpinner _rhoEntry;
    private JSpinner _visualStmSize;
    private JSpinner _verbalStmSize;
    private JSpinner _fieldOfView;

    private JPanel properties () {
      // -- create entry widgets
      _addLinkTime = new JSpinner (new SpinnerNumberModel (_model.getAddLinkTime (), 1, 100000, 1));
      _discriminationTime = new JSpinner (new SpinnerNumberModel (_model.getDiscriminationTime (), 1, 100000, 1));
      _familiarisationTime = new JSpinner (new SpinnerNumberModel (_model.getFamiliarisationTime (), 1, 100000, 1));
      _rhoEntry = new JSpinner (new SpinnerNumberModel (_model.getRho (), 0.0, 1.0, 0.1));
      _visualStmSize = new JSpinner (new SpinnerNumberModel (_model.getVisualStmSize (), 1, 10, 1));
      _verbalStmSize = new JSpinner (new SpinnerNumberModel (_model.getVerbalStmSize (), 1, 10, 1));
      _fieldOfView = new JSpinner (new SpinnerNumberModel (_model.getPerceiver().getFieldOfView (), 1, 100, 1));

      JPanel panel = new JPanel ();
      panel.setLayout (new GridLayout (7, 2));
      panel.add (new JLabel ("Add link time (ms)", SwingConstants.RIGHT));
      panel.add (_addLinkTime);
      panel.add (new JLabel ("Discrimination time (ms)", SwingConstants.RIGHT));
      panel.add (_discriminationTime);
      panel.add (new JLabel ("Familiarisation time (ms)", SwingConstants.RIGHT));
      panel.add (_familiarisationTime);
      panel.add (new JLabel ("Rho", SwingConstants.RIGHT));
      panel.add (_rhoEntry);
      panel.add (new JLabel ("Visual STM size", SwingConstants.RIGHT));
      panel.add (_visualStmSize);
      panel.add (new JLabel ("Verbal STM size", SwingConstants.RIGHT));
      panel.add (_verbalStmSize);
      panel.add (new JLabel ("Field of view", SwingConstants.RIGHT));
      panel.add (_fieldOfView);

      return panel;
    }
  }

  /**
   * Action to display information about the current model.
   */
  class ModelInformationAction extends AbstractAction implements ActionListener {
    private Shell _parent;

    ModelInformationAction (Shell parent) {
      super ("Information");

      _parent = parent;
    }

    public void actionPerformed (ActionEvent e) {
      JOptionPane.showMessageDialog (_parent,
          "<html><p>" + 
          "Nodes in LTM: " + _model.ltmSize () +
          "</p></html>",
          "Chrest: Model information",
          JOptionPane.INFORMATION_MESSAGE);
    }
  }

  /** 
   * Action to display a separate frame with information about the current model.
   * Frame is composed of separate views onto the model, all using the observer 
   * design pattern to keep updated as the model changes.
   */
  class ViewModelAction extends AbstractAction implements ActionListener {
    private Shell _parent;

    ViewModelAction (Shell parent) {
      super ("View");

      _parent = parent;
    }

    public void actionPerformed (ActionEvent e) {
      new ChrestView (_parent, _model);
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
    menu.add (new ClearModelAction (this));
    menu.add (new ModelPropertiesAction (this));
    menu.add (new JSeparator ());
    menu.add (new ModelInformationAction (this));
    menu.add (new ViewModelAction (this));

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

