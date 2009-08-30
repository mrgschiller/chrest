package jchrest.gui;

import jchrest.architecture.Chrest;
import jchrest.lib.ListPattern;

import java.awt.BorderLayout;
import java.awt.event.*;
import java.util.List;
import javax.swing.*;

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
  private JScrollPane constructPatternList () {
    _patternList = new JList (_patterns.toArray ());
    return new JScrollPane (_patternList);
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
            "for " +
            selectedPattern().toString ());
      }
    }
  }

  private Box constructButtons () {
    Box buttons = Box.createVerticalBox ();

    buttons.add (new JButton (new LearnPatternAction ()));
    buttons.add (new JButton (new LearnAllPatternAction ()));
    buttons.add (new JButton (new RecognisePatternAction ()));

    return buttons;
  }

  private JLabel _feedback;

  private JLabel constructFeedbackPanel () {
    _feedback = new JLabel ("FEEDBACK");
    return _feedback;
  }
}

