package jchrest.gui;

import jchrest.architecture.*;

import javax.swing.*;
import javax.swing.border.TitledBorder;

public class ChrestStmView extends JPanel {
  private Chrest _model;

  public ChrestStmView (Chrest model) {
    super ();

    _model = model;
    setBorder (new TitledBorder ("STM"));

    add (new JLabel ("STM"));

  }
}

