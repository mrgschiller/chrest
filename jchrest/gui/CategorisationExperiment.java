package jchrest.gui;

import jchrest.architecture.Chrest;
import jchrest.lib.ListPattern;
import jchrest.lib.PairedPattern;
import jchrest.lib.Pattern;

import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.*;

/**
 * This panel provides an interface for running categorisation experiments.
 *
 * @author Peter C. R. Lane
 */
public class CategorisationExperiment extends JPanel {
  private final Chrest _model;
  private final List<PairedPattern> _patterns;

  public CategorisationExperiment (Chrest model, List<PairedPattern> patterns) {
    super ();
    
    _model = model;
    _patterns = patterns;

    setLayout (new GridLayout (1, 1));
    JSplitPane jsp = new JSplitPane (JSplitPane.HORIZONTAL_SPLIT, createRunExperimentView (), createProtocolView ());
    jsp.setOneTouchExpandable (true);

    add (jsp);
  }

  private Map<PairedPattern, JCheckBox> _trainingSelection; 

  private JPanel createListView () {
    _trainingSelection = new HashMap<PairedPattern, JCheckBox> ();

    JPanel panel = new JPanel ();
    panel.setBorder (new TitledBorder ("Categorisation data"));
    panel.setLayout (new GridLayout (1, 1));

    JPanel pairsPanel = new JPanel ();
    pairsPanel.setLayout (new GridLayout (_patterns.size(), 3));
    for (PairedPattern pair : _patterns) {
      pairsPanel.add (new JLabel (pair.getFirst().toString ()));
      pairsPanel.add (new JLabel (pair.getSecond().toString ()));
      JCheckBox cb = new JCheckBox ("Training");
      cb.setSelected (true);
      pairsPanel.add (cb);
      _trainingSelection.put (pair, cb);
    }

    panel.add (new JScrollPane (pairsPanel));
    return panel;        
  } 

  private JCheckBox _randomOrder;
  private List<List<ListPattern>> _responses;
  private JScrollBar _protocolHorizontalBar;
  private JTable _protocol;

  class RestartAction extends AbstractAction implements ActionListener {
    RestartAction () {
      super ("Restart");
    }

    public void actionPerformed (ActionEvent e) {
      _model.clear ();
      _responses.clear ();

      updateControls ();
    }
  }

  class RunTrialAction extends AbstractAction implements ActionListener {
    RunTrialAction () {
      super ("Run Trial");
    }

    private List<PairedPattern> preparePatterns () {
      List<PairedPattern> patterns = new ArrayList<PairedPattern> ();
      java.util.Random gen = new java.util.Random ();
      for (PairedPattern pattern : _patterns) {
        if (_trainingSelection.get(pattern).isSelected ()) {
          if (_randomOrder.isSelected ()) {
            patterns.add (gen.nextInt (patterns.size () + 1), pattern);
          } else {
            patterns.add (pattern);
          }
        }
      }

      return patterns;
    }

    private void collectResponses () {
      List<ListPattern> responses = new ArrayList<ListPattern> ();
      for (PairedPattern pair : _patterns) {
        ListPattern response = _model.namePattern (pair.getFirst ());
        if (response != null) {
          responses.add (response);
        } else {
          responses.add (Pattern.makeVisualList (new String[]{"NONE"}));
        }
      }
      _responses.add (responses);
    }

    public void actionPerformed (ActionEvent e) {
      _model.freeze (); // save all gui updates to the end
      collectResponses ();
      for (PairedPattern pair : preparePatterns ()) {
        _model.learnAndNamePatterns (pair.getFirst (), pair.getSecond ());
      }
      updateControls ();
    }
  }

  private void updateControls () {
    ((AbstractTableModel)_protocol.getModel()).fireTableStructureChanged ();
    _model.unfreeze ();
  }

  private JPanel createControls () {
    _randomOrder = new JCheckBox ("Random order");
    _randomOrder.setToolTipText ("Set this to pass pairs to model in a random order");
        JButton restart = new JButton (new RestartAction ());
    restart.setToolTipText ("Reset the experiment and clear the model");
    JButton runTrial = new JButton (new RunTrialAction ());
    runTrial.setToolTipText ("Pass each stimulus-response pair once against the model");

    JPanel controls = new JPanel ();
    controls.setLayout (new GridLayout (2, 2, 10, 3));
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
          return "Source";
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
