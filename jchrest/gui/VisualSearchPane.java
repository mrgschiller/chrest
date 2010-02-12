package jchrest.gui;

import jchrest.architecture.Chrest;
import jchrest.lib.*;

import java.awt.*;
import java.awt.event.*;
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
  private Scenes _scenes;
  private SceneDisplay _sceneDisplay;

  public VisualSearchPane (Chrest model, Scenes scenes) {
    super ();

    _model = model;
    _scenes = scenes;
    _model.getPerceiver().setScene (_scenes.get (0));
    _sceneDisplay = new SceneDisplay (_scenes.get (0));

    setLayout (new BorderLayout ());

    add (_sceneDisplay);
    add (constructButtons (), BorderLayout.EAST);
    add (constructSelector (), BorderLayout.SOUTH);
  }

  private JComboBox _sceneSelector;

  private JPanel constructSelector () {
    JPanel panel = new JPanel ();
    panel.setLayout (new BorderLayout ());
    panel.add (new JLabel ("Scene: "), BorderLayout.WEST);

    _sceneSelector = new JComboBox (_scenes.getSceneNames ());
    _sceneSelector.addActionListener (new AbstractAction () {
      public void actionPerformed (ActionEvent e) {
        Scene newScene = _scenes.get (_sceneSelector.getSelectedIndex ());
        _model.getPerceiver().setScene (newScene);
        _sceneDisplay.updateScene (newScene);
      }
    });
    panel.add (_sceneSelector);

    return panel;
  }

  class StartAction extends AbstractAction implements ActionListener {
    private JLabel _lastHeuristic;

    public StartAction (JLabel lastHeuristic) {
      super ("Start");
      _lastHeuristic = lastHeuristic;
    }
    public void actionPerformed (ActionEvent e) {
      _model.getPerceiver().start ();
      _sceneDisplay.updateFixation (_model.getPerceiver().getFixationX (), 
          _model.getPerceiver().getFixationY (), 
          _model.getPerceiver().getFieldOfView ());
      _lastHeuristic.setText ("");
    }
  }

  class StepAction extends AbstractAction implements ActionListener {
    private JLabel _lastHeuristic;

    public StepAction (JLabel lastHeuristic) {
      super ("Step");
      _lastHeuristic = lastHeuristic;
    }
    public void actionPerformed (ActionEvent e) {
      _model.getPerceiver().moveEyeAndLearn ();
      _sceneDisplay.updateFixation (_model.getPerceiver().getFixationX (),
         _model.getPerceiver().getFixationY (),
         _model.getPerceiver().getFieldOfView ());
      _lastHeuristic.setText (_model.getHeuristicDescription ());
    }
  }

  private Box constructButtons () {
    JLabel heuristicLabel = new JLabel ("");

    Box buttons = Box.createVerticalBox ();
    JButton startButton = new JButton (new StartAction (heuristicLabel));
    JButton stepButton = new JButton (new StepAction (heuristicLabel));

    stepButton.setMaximumSize (stepButton.getPreferredSize ());

    buttons.add (Box.createGlue ());
    buttons.add (startButton);
    buttons.add (stepButton);
    buttons.add (heuristicLabel);
    buttons.add (Box.createGlue ());

    return buttons;
  }
}

class SceneDisplay extends JPanel {
  private Scene _scene;
  private SceneView _sceneView;

  public SceneDisplay (Scene scene) {
    super ();

    setLayout (new GridLayout (1, 1));
    setBorder (new TitledBorder ("Scene"));
    _scene = scene;
    _sceneView = new SceneView ();
    add (new JScrollPane (_sceneView));
  }

  public void updateScene (Scene scene) {
    _scene = scene;
    _sceneView.update ();
  }

  public void updateFixation (int x, int y, int fov) {
    _sceneView.setFixation (x, y, fov);
  }

  class SceneView extends JPanel {
    private int _maxX, _maxY;
    private int _fixationX, _fixationY, _fov;

    SceneView () {
      super ();

      setBackground (Color.WHITE);
      update ();
    }

    private int offsetX = 10;
    private int offsetY = 10;
    private int scale = 20;

    public void update () {

      _fixationX = _fixationY = -1; // set no fixation

      _maxX = scale * (_scene.getWidth () + 2); // + 2 for row heading and gaps
      _maxY = scale * (_scene.getHeight () + 2); // + 2 for column heading and gaps

      setPreferredSize (new Dimension (_maxX, _maxY));
      if (getParent () != null) ((JComponent)getParent ()).updateUI ();
      repaint ();
    }

    public void setFixation (int x, int y, int fov) {
      _fixationX = x;
      _fixationY = y;
      _fov = fov;
      repaint ();
    }

    // TODO: Add display of field of view
    public void paint (Graphics g) {
      super.paint (g); // make sure the background of the JPanel is drawn
      Graphics2D g2 = (Graphics2D)g;

      g2.setBackground (Color.WHITE);
      g2.clearRect (0, 0, _maxX, _maxY);

      // draw lines of grid
      for (int i = 0; i <= _scene.getHeight (); ++i) {
        g2.drawLine (offsetX, offsetY + scale * i, offsetX + scale * _scene.getWidth(), offsetY + scale * i);
      }
      for (int i = 0; i <= _scene.getWidth (); ++i) {
        g2.drawLine (offsetX + scale * i, offsetY, offsetX + scale * i, offsetY + scale * _scene.getHeight ());
      }
      // add row labels
      for (int i = 0; i < _scene.getHeight (); ++i) {
        g2.drawString ("" + (i+1), offsetX + scale * (_scene.getWidth() + 1), offsetY + scale * (i+1) - 5);
      }
      // add column labels
      for (int i = 0; i < _scene.getWidth (); ++i) {
        g2.drawString ("" + (i+1), offsetX + scale * i + 5, offsetY + scale * (_scene.getHeight() + 1));
      }
      
      // draw entries within grid
      for (int i = 0; i < _scene.getHeight (); ++i) {
        for (int j = 0; j < _scene.getWidth (); ++j) {
          if (!_scene.isEmpty (i, j)) {
            g2.drawString (_scene.getItem (i, j), offsetX + 5 + scale * j, offsetY + scale - 5 + scale * i);
          }
        }
      }

      // draw fixation
      if (_fixationX >= 0 && _fixationY >= 0) {
        g2.setColor (Color.BLUE);
        g2.setStroke (new BasicStroke (2));
        g2.drawRoundRect (offsetX + scale * _fixationX, offsetY + scale * _fixationY,
            scale, scale, 5, 5);
        g2.drawRoundRect (offsetX + scale * (_fixationX - _fov), offsetY + scale * (_fixationY - _fov), 
            scale * (_fov * 2 + 1), scale * (_fov * 2 + 1),
            5, 5);
      }
    }
  }
}
