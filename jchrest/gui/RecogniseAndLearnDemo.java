package jchrest.gui;

import jchrest.architecture.Chrest;
import jchrest.lib.ListPattern;

import java.awt.BorderLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.*;
import java.util.List;
import javax.swing.*;
import javax.swing.border.*;

/**
 * This panel provides an interface for a simple demonstration of recognising 
 * and learning the given set of panels.
 *
 * @author Peter C. R. Lane
 */
class RecogniseAndLearnDemo extends JPanel {
  private Chrest _model;
  private List<ListPattern> _patterns;

  public RecogniseAndLearnDemo (Chrest model, List<ListPattern> patterns) {
    _model = model;
    _patterns = patterns;

    setLayout (new BorderLayout ());
    add (constructPatternList (), BorderLayout.CENTER);
    add (constructFeedbackPanel (), BorderLayout.SOUTH);
    add (constructButtons (), BorderLayout.EAST);
  }

  private JList _patternList;
  private JPanel constructPatternList () {
    JPanel panel = new JPanel ();
    panel.setBorder (new TitledBorder ("Patterns"));
    panel.setLayout (new GridLayout (1, 1));

    _patternList = new JList (_patterns.toArray ());
    _patternList.setSelectedIndex (0);

    panel.add (new JScrollPane (_patternList));

    return panel;
  }

  abstract class PatternAction extends AbstractAction implements ActionListener {
    PatternAction (String label) {
      super (label);
    }

    boolean isSelected () {
      return (_patternList.getSelectedIndex () != -1);
    }

    ListPattern selectedPattern () {
      ListPattern pattern = null;
      if (isSelected ()) {
        pattern = (ListPattern)_patternList.getSelectedValue ();
      }
      return pattern;
    }

    public abstract void actionPerformed (ActionEvent e);
  }

  class LearnPatternAction extends PatternAction {
    LearnPatternAction () {
      super ("Learn");
    }

    public void actionPerformed (ActionEvent e) {
      if (isSelected ()) {
        _model.recogniseAndLearn (selectedPattern());
        _feedback.setText ("Learning " + selectedPattern().toString ());
      }
    }
  }

  class LearnAllPatternAction extends AbstractAction implements ActionListener {
    LearnAllPatternAction () {
      super ("Learn all"); 
    }

    public void actionPerformed (ActionEvent e) {
      for (ListPattern pattern : _patterns) {
        _model.recogniseAndLearn (pattern);
      }
      _feedback.setText ("Learnt all patterns");
    }
  }

  class RecognisePatternAction extends PatternAction {
    RecognisePatternAction () {
      super ("Recognise");
    }

    public void actionPerformed (ActionEvent e) {
      if (isSelected ()) {
        _feedback.setText ("Recalled " + 
            _model.recallPattern (selectedPattern()).toString () +
            " for " +
            selectedPattern().toString ());
      }
    }
  }

  private Box constructButtons () {
    Box buttons = Box.createVerticalBox ();
    JButton learnButton = new JButton (new LearnPatternAction ());
    JButton learnAllButton = new JButton (new LearnAllPatternAction ());
    JButton recogniseButton = new JButton (new RecognisePatternAction ());
    
    learnButton.setToolTipText ("Train model on currently selected pattern");
    learnAllButton.setToolTipText ("Train model on all patterns");
    recogniseButton.setToolTipText ("Recall currently selected pattern from model");

    learnButton.setMaximumSize (recogniseButton.getPreferredSize ());
    learnAllButton.setMaximumSize (recogniseButton.getPreferredSize ());

    buttons.add (Box.createGlue ());
    buttons.add (learnButton);
    buttons.add (learnAllButton);
    buttons.add (recogniseButton);
    buttons.add (Box.createGlue ());

    return buttons;
  }

  private JLabel _feedback;

  private JLabel constructFeedbackPanel () {
    _feedback = new JLabel ("FEEDBACK");
    _feedback.setFont (new Font ("Arial", Font.PLAIN, 18));
    _feedback.setBorder (new EmptyBorder (10, 50, 10, 50));
    return _feedback;
  }
}

