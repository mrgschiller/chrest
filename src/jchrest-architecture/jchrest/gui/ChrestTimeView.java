package jchrest.gui;

import jchrest.architecture.Chrest;

import javax.swing.*;
import javax.swing.border.TitledBorder;

/**
  * This panel displays the model's current clock time.
  *
  * @author Peter C. R. Lane
  */
public class ChrestTimeView extends JPanel {
  private Chrest _model;
  private JLabel _display;

  public ChrestTimeView (Chrest model) {
    super ();

    setBorder (new TitledBorder ("Clock"));

    _model = model;
    _display = new JLabel ("Time (ms): " + _model.getClock ());

    add (_display);
  }

  public void update () {
    _display.setText ("Time (ms): " + _model.getClock ());
  }
}
