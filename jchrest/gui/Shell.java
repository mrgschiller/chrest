package jchrest.gui;

import jchrest.architecture.Chrest;
import jchrest.lib.FileUtilities;
import jchrest.lib.ListPattern;
import jchrest.lib.PairedPattern;
import jchrest.lib.ParsingErrorException;
import jchrest.lib.Pattern;
import jchrest.lib.Scenes;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.*;
import java.io.*;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.*;

import org.jfree.chart.*;
import org.jfree.chart.plot.*;
import org.jfree.data.*;
import org.jfree.data.statistics.*;

/**
 * The main frame for the Chrest shell.
 *
 * @author Peter C. R. Lane
 */
public class Shell extends JFrame {
  private Chrest _model;

  private Shell () {
    super ("CHREST");

    _model = new Chrest ();

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    createMenuBar ();

    setContentPane( new JLabel ("Open some data to use shell") );

    setSize(600, 400);
    setLocationRelativeTo (null);
    setTheme ("Nimbus");
    setVisible (true);
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
          "<P><P>Copyright (c) 2010-11, Peter C. R. Lane.</P></P>" +
          "<P>Released under GNU General Public License</a>, version 3.</P>" + 
          
          "<p>See <a href=\"http://chrest.info\">http://chrest.info</a> for more information.</P></HTML>",
          "About Chrest Shell v. 0.15", 
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
      (new LoadDataThread (_parent)).execute ();
    }
  }

  /**
   * Worker thread to handle loading the data.
   */
  private class LoadDataThread extends SwingWorker<Void, Void> {
    private Shell _parent;
    private String _task;
    private List<ListPattern> _items;
    private List<PairedPattern> _pairs;
    private Scenes _scenes;
    private int _status;

    LoadDataThread (Shell parent) {
      _parent = parent;
      _task = "";
      _items = null;
      _pairs = null;
      _scenes = null;
    }

    @Override
      public Void doInBackground () {
        File file = FileUtilities.getLoadFilename (_parent);
        if (file == null) {
          _status = 3;
        } else {
          try {
            _status = 0; // assume all will be fine
            _task = "";
            // add a monitor to the input stream, to show a message if input is taking a while
            InputStream inputStream = new ProgressMonitorInputStream(
                _parent, 
                "Reading the input file", 
                new FileInputStream (file));
            BufferedReader input = new BufferedReader (new InputStreamReader (inputStream));

            String line = input.readLine ();
            if (line != null) {
              _task = line.trim ();
            }

            if (_task.equals ("recognise-and-learn")) {
              _items = readItems (input, false);
            } else if (_task.equals ("serial-anticipation")) {
              _items = readItems (input, true);
            } else if (_task.equals ("paired-associate")) {
              _pairs = readPairedItems (input, false);
            } else if (_task.equals ("categorisation")) {
              _pairs = readPairedItems (input, true);
            } else if (_task.equals ("visual-search")) {
              _scenes = Scenes.read (input); // throws IOException if any problem
            } else if (_task.equals ("visual-search-with-move")){
              _scenes = Scenes.readWithMove (input); // throws IOException if any problem
            }
          } catch (InterruptedIOException ioe) {
            _status = 2; // flag cancelled error
          } catch (IOException ioe) {
            _status = 1; // flag an IO error
          }
        }
        return null;
      }

    @Override
      protected void done () {
        if (_status == 3) {
          ; // do nothing
        } else if (_status == 1) {
          JOptionPane.showMessageDialog (_parent, 
              "There was an error in processing your file", 
              "File error",
              JOptionPane.ERROR_MESSAGE);
        } else if (_status == 2) {
          JOptionPane.showMessageDialog (_parent, 
              "You cancelled the operation : no change", 
              "File Load Cancelled",
              JOptionPane.WARNING_MESSAGE);
        } else { // (_status == 0)
          if (_task.equals ("recognise-and-learn") && _items != null) {
            _parent.setContentPane (new RecogniseAndLearnDemo (_model, _items));
          } else if (_task.equals ("serial-anticipation") && _items != null) {
            _parent.setContentPane (new PairedAssociateExperiment (_model, PairedAssociateExperiment.makePairs(_items)));
          } else if (_task.equals ("paired-associate") && _pairs != null) {
            _parent.setContentPane (new PairedAssociateExperiment (_model, _pairs));
          } else if (_task.equals ("categorisation") && _pairs != null) {
            _parent.setContentPane (new CategorisationExperiment (_model, _pairs));
          } else if (_task.equals ("visual-search") && _scenes != null) {
            _parent.setContentPane (new VisualSearchPane (_model, _scenes));
          } else {
            JOptionPane.showMessageDialog (_parent,
                "Invalid task on first line of file",
                "File error",
                JOptionPane.ERROR_MESSAGE);
          }
          _parent.validate ();
        }
      }

    private List<ListPattern> readItems (BufferedReader input, boolean verbal) throws IOException {
      List<ListPattern> items = new ArrayList<ListPattern> ();
      String line = input.readLine ();

      while (line != null) {
        ListPattern pattern;
        if (verbal) {
          pattern = Pattern.makeVerbalList (line.trim().split("[, ]"));
        } else {
          pattern = Pattern.makeVisualList (line.trim().split("[, ]"));
        }
        pattern.setFinished ();
        items.add (pattern);
        line = input.readLine ();
      } 

      return items;
    }

    // categorisation = false => make both verbal
    // categorisation = true  => make first visual, second verbal
    private List<PairedPattern> readPairedItems (BufferedReader input, boolean categorisation) throws IOException {
      List<PairedPattern> items = new ArrayList<PairedPattern> ();
      String line = input.readLine ();
      while (line != null) {
        String[] pair = line.split (":");
        if (pair.length != 2) throw new IOException (); // malformed pair
        ListPattern pat1;
        if (categorisation) {
          pat1 = Pattern.makeVisualList (pair[0].trim().split("[, ]"));
        } else {
          pat1 = Pattern.makeVerbalList (pair[0].trim().split("[, ]"));
        }
        pat1.setFinished ();
        ListPattern pat2 = Pattern.makeVerbalList (pair[1].trim().split("[, ]"));
        pat2.setFinished ();
        items.add (new PairedPattern (pat1, pat2));

        line = input.readLine ();
      }

      return items;
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
        _model.setCreateTemplates(_createTemplates.isSelected ());
        _model.setSimilarityThreshold(((SpinnerNumberModel)_similarityThreshold.getModel()).getNumber().intValue ());
      }
    }

    private JSpinner _addLinkTime;
    private JSpinner _discriminationTime;
    private JSpinner _familiarisationTime;
    private JSpinner _rhoEntry;
    private JSpinner _visualStmSize;
    private JSpinner _verbalStmSize;
    private JSpinner _fieldOfView;
    private JCheckBox _createTemplates;
    private JSpinner _similarityThreshold;

    private JPanel properties () {
      // -- create entry widgets
      _addLinkTime = new JSpinner (new SpinnerNumberModel (_model.getAddLinkTime (), 1, 100000, 1));
      _discriminationTime = new JSpinner (new SpinnerNumberModel (_model.getDiscriminationTime (), 1, 100000, 1));
      _familiarisationTime = new JSpinner (new SpinnerNumberModel (_model.getFamiliarisationTime (), 1, 100000, 1));
      _rhoEntry = new JSpinner (new SpinnerNumberModel (_model.getRho (), 0.0, 1.0, 0.1));
      _visualStmSize = new JSpinner (new SpinnerNumberModel (_model.getVisualStmSize (), 1, 10, 1));
      _verbalStmSize = new JSpinner (new SpinnerNumberModel (_model.getVerbalStmSize (), 1, 10, 1));
      _fieldOfView = new JSpinner (new SpinnerNumberModel (_model.getPerceiver().getFieldOfView (), 1, 100, 1));
      _similarityThreshold = new JSpinner (new SpinnerNumberModel (_model.getSimilarityThreshold (), 1, 100, 1));
      _createTemplates = new JCheckBox ("Use templates", _model.getCreateTemplates ());

      JPanel panel = new JPanel ();
      panel.setLayout (new GridLayout (9, 2));
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
      panel.add (new JLabel ("Similarity threshold", SwingConstants.RIGHT));
      panel.add (_similarityThreshold);
      panel.add (new JLabel (""));
      panel.add (_createTemplates);

      return panel;
    }
  }

  /**
   * Action to save test-link data from current model in VNA format.
   */
  class SaveModelAsVnaAction extends AbstractAction implements ActionListener {
    private Shell _parent;

    SaveModelAsVnaAction (Shell parent) {
      super ("Save visual network (.VNA)"); 

      _parent = parent;
    }

    public void actionPerformed (ActionEvent e) {
      File file = FileUtilities.getSaveFilename (_parent, "Save visual network");
      if (file == null) return;
      try {
        FileWriter writer = new FileWriter (file);
        _model.writeModelAsVna (writer);
        writer.close ();
      } catch (IOException ioe) {
        JOptionPane.showMessageDialog (_parent,
            "File " + file.getName () + 
            " could not be saved due to an error.",
            "Error: File save error",
            JOptionPane.ERROR_MESSAGE);
      }
    }
  }

  /**
   * Action to save similarity links in current model as VNA.
   */
  class SaveModelSimilaritiesAsVnaAction extends AbstractAction implements ActionListener {
    private Shell _parent;

    SaveModelSimilaritiesAsVnaAction (Shell parent) {
      super ("Save visual similarity links (.VNA)"); 

      _parent = parent;
    }

    public void actionPerformed (ActionEvent e) {
      File file = FileUtilities.getSaveFilename (_parent, "Save visual similarity links");
      if (file == null) return;
      try {
        FileWriter writer = new FileWriter (file);
        _model.writeModelSimilarityLinksAsVna (writer);
        writer.close ();
      } catch (IOException ioe) {
        JOptionPane.showMessageDialog (_parent,
            "File " + file.getName () + 
            " could not be saved due to an error.",
            "Error: File save error",
            JOptionPane.ERROR_MESSAGE);
      }
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
      JPanel base = new JPanel ();
      base.setLayout (new GridLayout(1,1));

      JTabbedPane jtb = new JTabbedPane ();
      jtb.addTab ("Info", getInfoPane ());
      jtb.addTab ("Contents", getHistogramPane (_model.getContentCounts(), "contents", "Histogram of Contents Sizes", "Contents size"));
      jtb.addTab ("Images", getHistogramPane (_model.getImageCounts(), "images", "Histogram of Image Sizes", "Image size"));
      jtb.addTab ("Similarity", getHistogramPane (_model.getSimilarityCounts(), "similarity", "Histogram of Number of Similarity Links", "Number of similarity links"));
      base.add (jtb);

      JOptionPane pane = new JOptionPane (base, JOptionPane.INFORMATION_MESSAGE);
      JDialog dialog = pane.createDialog (_parent, "Chrest: Model information");
      dialog.setResizable (true);
      dialog.setVisible (true);
    }
  }

  private JLabel getInfoPane () {
    DecimalFormat twoPlaces = new DecimalFormat("0.00");
    return new JLabel (
        "<html><p>" + 
        "Total nodes in LTM: " + _model.getTotalLtmNodes () +
        "<hr>" + 
        "Visual nodes: " + _model.ltmVisualSize () + 
        " Average depth: " + twoPlaces.format (_model.getVisualLtmAverageDepth ()) +
        "<br>Verbal nodes: " + _model.ltmVerbalSize () + 
        " Average depth: " + twoPlaces.format (_model.getVerbalLtmAverageDepth ()) +
        "<br>Action nodes: " + _model.ltmActionSize () + 
        " Average depth: " + twoPlaces.format (_model.getActionLtmAverageDepth ()) +
        "<br>Number of templates: " + _model.countTemplates () +
        "</p></html>"
        );
  }

  private JPanel getHistogramPane (Map<Integer, Integer> contentSizes, String label, String title, String xAxis) {
    int largest = 0;
    for (Integer key : contentSizes.keySet ()) {
      if (key > largest) largest = key;
    }
    SimpleHistogramDataset dataset = new SimpleHistogramDataset (label);
    for (int i = 0; i <= largest; ++i) {
      SimpleHistogramBin bin = new SimpleHistogramBin ((double)i, (double)(i+1), true, false);
      int count = 0;
      if (contentSizes.containsKey (i)) {
        count = contentSizes.get (i);
      }
      bin.setItemCount (count);
      dataset.addBin (bin);
    }
    PlotOrientation orientation = PlotOrientation.VERTICAL; 
    boolean show = false; 
    boolean toolTips = true;
    boolean urls = false; 
    JFreeChart chart = ChartFactory.createHistogram( title, xAxis, "frequency", 
        dataset, orientation, show, toolTips, urls);

    JPanel panel = new JPanel ();
    panel.setLayout (new BorderLayout ());
    panel.add (new ChartPanel (chart, 400, 300, 200, 100, 600, 600, true, true, true, true, true, true));
    JButton saveButton = new JButton ("Save Data");
    saveButton.setToolTipText ("Save the histogram data to a CSV file");
    saveButton.addActionListener ( new SaveHistogramActionListener (this, contentSizes));
    panel.add (saveButton, BorderLayout.SOUTH);
    return panel;
  }

  class SaveHistogramActionListener implements ActionListener {
    Shell _parent;
    Map<Integer, Integer> _data;

    public SaveHistogramActionListener (Shell parent, Map<Integer, Integer> data) {
      _parent = parent;
      _data = data;
    }
    
    public void actionPerformed (ActionEvent e) {
      File file = FileUtilities.getSaveFilename (_parent, "Save histogram data");
      if (file == null) return;
      try {
        FileWriter writer = new FileWriter (file);
        for (Integer key : _data.keySet ()) {
          writer.write ("" + key + ", " + _data.get (key) + "\n");
        }
        writer.close ();
      } catch (IOException ioe) {
        JOptionPane.showMessageDialog (_parent,
            "File " + file.getName () + 
            " could not be saved due to an error.",
            "Error: File save error",
            JOptionPane.ERROR_MESSAGE);
      }
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

    JMenu submenu = new JMenu ("Save");
    submenu.add (new SaveModelAsVnaAction (this));
    submenu.add (new SaveModelSimilaritiesAsVnaAction (this));
    menu.add (submenu);

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
    // SwingUtilities.updateComponentTreeUI(_fileChooser); TODO: update FileUtilities filechooser
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

