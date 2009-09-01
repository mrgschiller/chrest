package jchrest.gui;

import jchrest.architecture.Chrest;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * This panel provides an interface for training and recall of 
 * models on visual search problems.
 *
 * @author Peter C. R. Lane
 */
public class VisualSearchPane extends JPanel {
  private Chrest _model;

  public VisualSearchPane (Chrest model) {
    super ();

    _model = model;
    setLayout (new BorderLayout ());

    add (new SceneDisplay ());
    add (constructButtons (), BorderLayout.EAST);
    add (constructSelector (), BorderLayout.SOUTH);
  }

  private JComboBox _sceneSelector;

  private JPanel constructSelector () {
    JPanel panel = new JPanel ();
    panel.setLayout (new BorderLayout ());
    panel.add (new JLabel ("Scene: "), BorderLayout.WEST);

    _sceneSelector = new JComboBox (new String[]{"Scene 1", "Scene 2", "Scene 3"});
    panel.add (_sceneSelector);

    return panel;
  }

  private Box constructButtons () {
    Box buttons = Box.createVerticalBox ();
    JButton startButton = new JButton ("Start");
    JButton stepButton = new JButton ("Step");

    stepButton.setMaximumSize (stepButton.getPreferredSize ());

    buttons.add (Box.createGlue ());
    buttons.add (startButton);
    buttons.add (stepButton);
    buttons.add (Box.createGlue ());

    return buttons;
  }
}

class SceneDisplay extends JPanel {

  public SceneDisplay () {
    super ();

    setLayout (new GridLayout (1, 1));
    setBorder (new TitledBorder ("Scene"));

    add (new JLabel ("Current scene"));
  }
}
