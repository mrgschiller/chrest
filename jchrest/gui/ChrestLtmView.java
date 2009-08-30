package jchrest.gui;

import jchrest.architecture.*;
import javax.swing.*;
import javax.swing.border.*;

public class ChrestLtmView extends JPanel {
  private Chrest _model;

  public ChrestLtmView (Chrest model) {
    super ();

    _model = model;
    setBorder (new TitledBorder ("LTM"));
    setLayout (new BorderLayout ());

  }


}

