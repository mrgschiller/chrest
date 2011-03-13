package jchrest.gui;

import jchrest.architecture.Chrest;
import jchrest.lib.ListPattern;
import jchrest.lib.PairedPattern;
import jchrest.lib.Pattern;

import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.*;

/**
 * This panel provides an interface for running paired associate
 * experiments.
 * 
 * @author Peter C. R. Lane
 */
public class PairedAssociateExperiment extends JPanel {
  private Chrest _model;
  private List<PairedPattern> _patterns;

  public PairedAssociateExperiment (Chrest model, List<PairedPattern> patterns) {
    super ();
    
    _model = model;
    _patterns = patterns;

    setLayout (new GridLayout (1, 1));
    JSplitPane jsp = new JSplitPane (JSplitPane.HORIZONTAL_SPLIT, createRunExperimentView (), createProtocolView ());
    jsp.setOneTouchExpandable (true);

    add (jsp);
  }

  /**
   * Convert a list of ListPatterns into a list of stimulus-response pairs.
   */
  public static List<PairedPattern> makePairs (List<ListPattern> patterns) {
    List<PairedPattern> pairs = new ArrayList<PairedPattern> ();
    for (int i = 1; i < patterns.size (); ++i) {
      pairs.add (new PairedPattern (patterns.get(i-1), patterns.get(i)));
    }

    return pairs;
  }

  private JPanel createListView () {
    JPanel panel = new JPanel ();
    panel.setBorder (new TitledBorder ("Stimulus-response pairs"));
    panel.setLayout (new GridLayout (1, 1));

    JPanel pairsPanel = new JPanel ();
    pairsPanel.setLayout (new GridLayout (_patterns.size(), 2));
    for (PairedPattern pair : _patterns) {
      pairsPanel.add (new JLabel (pair.getFirst().toString ()));
      pairsPanel.add (new JLabel (pair.getSecond().toString ()));
    }

    panel.add (new JScrollPane (pairsPanel));
    return panel;        
  }

  class RestartAction extends AbstractAction implements ActionListener {
    RestartAction () {
      super ("Restart");
    }

    public void actionPerformed (ActionEvent e) {
      _model.clear ();
      _responses.clear ();
      _exptClock = 0;

      updateControls ();
    }
  }

  private List<List<ListPattern>> _responses;

  class RunTrialAction extends AbstractAction implements ActionListener {
    RunTrialAction () {
      super ("Run Trial");

      _exptClock = 0;
    }

    private List<PairedPattern> preparePatterns () {
      List<PairedPattern> patterns = new ArrayList<PairedPattern> ();
      java.util.Random gen = new java.util.Random ();
      for (PairedPattern pattern : _patterns) {
        if (_randomOrder.isSelected ()) {
          patterns.add (gen.nextInt (patterns.size () + 1), pattern);
        } else {
          patterns.add (pattern);
        }
      }

      return patterns;
    }

    private void collectResponses () {
      List<ListPattern> responses = new ArrayList<ListPattern> ();
      for (PairedPattern pair : _patterns) {
        ListPattern response = _model.followPattern (pair.getFirst ());
        if (response != null) {
          responses.add (response);
        } else {
          responses.add (Pattern.makeVisualList (new String[]{"NONE"}));
        }
      }
      _responses.add (responses);
    }

    public void actionPerformed (ActionEvent e) {
      collectResponses ();
      for (PairedPattern pair : preparePatterns ()) {
        _model.learnAndLinkPatterns (pair.getFirst (), pair.getSecond (), _exptClock);
        _exptClock += ((SpinnerNumberModel)_interItemTime.getModel()).getNumber().intValue ();
      }
      _exptClock += ((SpinnerNumberModel)_endTrialTime.getModel()).getNumber().intValue ();
      updateControls ();
    }
  }

  private int _exptClock;
  private JLabel _experimentTimeLabel;
  private JSpinner _endTrialTime;
  private JSpinner _interItemTime;
  private JCheckBox _randomOrder;
  private JScrollBar _protocolHorizontalBar;
  private JTable _protocol;

  private void updateControls () {
    ((AbstractTableModel)_protocol.getModel()).fireTableStructureChanged ();
    _experimentTimeLabel.setText ("" + _exptClock);
  }

  private JPanel createControls () {
    _experimentTimeLabel = new JLabel ("0");
    _endTrialTime = new JSpinner (new SpinnerNumberModel (2000, 1, 50000, 1));
    _interItemTime = new JSpinner (new SpinnerNumberModel (2000, 1, 50000, 1));
    _randomOrder = new JCheckBox ("Random order");
    _randomOrder.setToolTipText ("Set this to pass pairs to model in a random order");
    JButton restart = new JButton (new RestartAction ());
    restart.setToolTipText ("Reset the experiment and clear the model");
    JButton runTrial = new JButton (new RunTrialAction ());
    runTrial.setToolTipText ("Pass each stimulus-response pair once against the model");

    JPanel controls = new JPanel ();
    controls.setLayout (new GridLayout (5, 2, 10, 3));
    controls.add (new JLabel ("Experiment time (ms)", SwingConstants.RIGHT));
    controls.add (_experimentTimeLabel);
    controls.add (new JLabel ("End trial time (ms)", SwingConstants.RIGHT));
    controls.add (_endTrialTime);
    controls.add (new JLabel ("Inter item time (ms)", SwingConstants.RIGHT));
    controls.add (_interItemTime);
    controls.add (_randomOrder);
    controls.add (restart);
    controls.add (new JLabel (""));
    controls.add (runTrial);

    return controls;
  }

  private JPanel createRunExperimentView () {
    JPanel panel = new JPanel ();
    panel.setLayout (new BorderLayout ());
    panel.add (createListView ());
    panel.add (createControls (), BorderLayout.SOUTH);

    return panel;
  }

  private void createProtocolTable () {
    TableModel tm = new AbstractTableModel () {
      // include a row for the number of errors
      public int getRowCount () {
        return 1 + _patterns.size ();
      }
      // include two columns for the stimulus and target response
      public int getColumnCount () {
        return 2 + _responses.size (); 
      }
      // compute the total number of errors in given column of responses
      private int getErrors (int trial) {
        int errors = 0;
        for (int i = 0, n = _patterns.size (); i < n; ++i) {
          if (_responses.get(trial).get(i).equals(_patterns.get(i).getSecond ())) {
          } else {
            errors += 1;
          }
        }
        return errors;
      }
      public Object getValueAt (int row, int column) {
        if (column == 0) {
          if (row == _patterns.size ()) {
            return "";
          } else {
            return _patterns.get(row).getFirst ();
          }
        } else if (column == 1) {
          if (row == _patterns.size ()) {
            return "Errors:";
          } else {
            return _patterns.get(row).getSecond ();
          }
        } else {
          if (row == _patterns.size ()) {
            return "" + getErrors (column-2);
          } else {
            return _responses.get(column-2).get(row).toString ();
          }
        }
      }
      public String getColumnName (int column) {
        if (column == 0) {
          return "Stimulus";
        } else if (column == 1) {
          return "Target";
        } else {
          return "Trial " + (column - 1);
        }
      }
      public void fireTableStructureChanged() {
        super.fireTableStructureChanged ();
        _protocolHorizontalBar.setValue (_protocolHorizontalBar.getMaximum ());
      }
    };
    _protocol = new JTable (tm);
    _protocol.setAutoResizeMode (JTable.AUTO_RESIZE_OFF);
  }

  private JPanel createProtocolView () {
    JPanel panel = new JPanel ();
    panel.setBorder (new TitledBorder ("Protocol"));
    panel.setLayout (new GridLayout (1, 1));
    _responses = new ArrayList<List<ListPattern>> ();
    createProtocolTable ();
    JScrollPane scrollPane = new JScrollPane (_protocol);
    _protocolHorizontalBar = scrollPane.getHorizontalScrollBar ();
    panel.add (scrollPane);

    return panel;
  }
}

