package jchrest.gui;

import jchrest.architecture.Chrest;
import jchrest.architecture.Node;
import jchrest.lib.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.*;
import javax.swing.border.*;

// Import the JFreeChart classes
import org.jfree.chart.*;
import org.jfree.chart.plot.*;
import org.jfree.data.*;
import org.jfree.data.general.*;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.ui.RectangleInsets;

/**
 * This panel provides an interface for building and testing  
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
    _domainSelector = new JComboBox (new String[]{"Generic", "Chess"});
    _domainSelector.addActionListener (new AbstractAction () {
      public void actionPerformed (ActionEvent e){
        int index = _domainSelector.getSelectedIndex ();
        if (index == 0) {
          _model.setDomain (new GenericDomain ());
        } else { // if (index == 1) 
          _model.setDomain (new ChessDomain ());
        }
      }
    });

    JTabbedPane jtb = new JTabbedPane ();
    jtb.addTab ("Train", trainPanel ());
    jtb.addTab ("Recall", recallPanel ());
    jtb.addTab ("Log", logPanel ());
    jtb.addTab ("Analyse", analysePanel ());

    setLayout (new BorderLayout ());
    add (jtb);
  }

  // -- set up the training panel
  private JPanel trainPanel () {
    JPanel panel = new JPanel ();
    panel.setLayout (new BoxLayout (panel, BoxLayout.Y_AXIS));

    panel.add (constructTrainingOptions ());
    panel.add (runTrainingButtons ());
    if (freeChartLoaded ()) {
      panel.add (createPanel ());
    }

    return panel;
  }

  private JSpinner _maxTrainingCycles;
  private JSpinner _numFixations;
  private JSpinner _maxNetworkSize;

  private JPanel constructTrainingOptions () {
    _maxTrainingCycles = new JSpinner (new SpinnerNumberModel (5, 1, 1000, 1));
    _numFixations = new JSpinner (new SpinnerNumberModel (20, 1, 100, 1));
    _maxNetworkSize = new JSpinner (new SpinnerNumberModel (100000, 1, 10000000, 1));

    JPanel panel = new JPanel ();
    panel.setLayout (new GridLayout (5, 2));
    panel.add (new JLabel ("Domain of scenes: ", SwingConstants.RIGHT));
    panel.add (_domainSelector);
    panel.add (new JLabel ("Number of scenes: ", SwingConstants.RIGHT));
    panel.add (new JLabel ("" + _scenes.size ()));
    panel.add (new JLabel ("Maximum training cycles: ", SwingConstants.RIGHT));
    panel.add (_maxTrainingCycles);
    panel.add (new JLabel ("Number of fixations per scene: ", SwingConstants.RIGHT));
    panel.add (_numFixations);
    panel.add (new JLabel ("Maximum network size: ", SwingConstants.RIGHT));
    panel.add (_maxNetworkSize);

    return panel;
  }

  // confirm if FreeChart has been included in classpath
  private boolean freeChartLoaded () {
    try {
      new XYSeriesCollection ();

      return true;
    } catch (NoClassDefFoundError ex) {
      return false;
    }
  }

  private JPanel runTrainingButtons () {
    JPanel panel = new JPanel ();

    _trainingTimes = new XYSeries ("CHREST model");
    _trainAction = new TrainAction ();
    JButton trainButton = new JButton (_trainAction);
    trainButton.setToolTipText ("Train a fresh CHREST model up to the given network size");
    _stopAction = new StopAction ();
    _stopAction.setEnabled (false);
    JButton stopButton = new JButton (_stopAction);
    stopButton.setToolTipText ("Stop the current training");
    _feedback = new JProgressBar (0, 100);
    _feedback.setToolTipText ("Proportion of target network size");
    _feedback.setValue (0);
    _feedback.setStringPainted (true);
    panel.add (trainButton);
    panel.add (stopButton);
    panel.add (_feedback);

    return panel;
  }

  private XYSeries _trainingTimes;

  private ChartPanel createPanel () {
    XYSeriesCollection dataset = new XYSeriesCollection ();
    dataset.addSeries (_trainingTimes);

    JFreeChart chart = ChartFactory.createXYLineChart(
        "Plot of network size vs. number of training patterns",
        "Number of training patterns",
        "Network size",
        dataset, 
        org.jfree.chart.plot.PlotOrientation.VERTICAL,
        true, 
        true, 
        false
        );

    XYPlot plot = (XYPlot) chart.getPlot();
    plot.setBackgroundPaint(Color.lightGray);
    plot.setDomainGridlinePaint(Color.white);
    plot.setRangeGridlinePaint(Color.white);
    plot.setAxisOffset(new RectangleInsets(5.0, 5.0, 5.0, 5.0));
    plot.setDomainCrosshairVisible(true);
    plot.setRangeCrosshairVisible(true);

    XYItemRenderer r = plot.getRenderer();
    if (r instanceof XYLineAndShapeRenderer) {
      XYLineAndShapeRenderer renderer = (XYLineAndShapeRenderer) r;
      renderer.setBaseShapesVisible(true);
      renderer.setBaseShapesFilled(true);
      renderer.setDrawSeriesLineAsPath(true);
    }

    return new ChartPanel (chart);
  }

  private class Pair {
    private int _first, _second;
    Pair (int first, int second) {
      _first = first;
      _second = second;
    }
    int getFirst () {
      return _first;
    }
    int getSecond () {
      return _second;
    }
  }

  private class TrainingThread extends SwingWorker<List<Pair>, Pair> {
    private Chrest _model;
    private Scenes _scenes;
    private int _maxCycles, _maxSize, _numFixations;

    TrainingThread (Chrest model, Scenes scenes, int maxCycles, int maxSize, int numFixations) {
      _model = model;
      _scenes = scenes;
      _maxCycles = maxCycles;
      _maxSize = maxSize;
      _numFixations = numFixations;
    }

    @Override
      public List<Pair> doInBackground () {
        _model.clear ();
        _model.freeze ();
        List<Pair> results = new ArrayList<Pair> ();
        Pair result = new Pair (0, 0);
        results.add (result);
        publish (result);

        int stepSize = (_maxCycles * _scenes.size ()) / 100;

        int cycle = 0;
        int positionsSeen = 0;
        while (
            (cycle < _maxCycles) && 
            (_model.getTotalLtmNodes () < _maxSize) &&
            !isCancelled ()) {
          for (int i = 0; i < _scenes.size () && (_model.getTotalLtmNodes () < _maxSize) && !isCancelled (); i++) {
            _model.learnScene (_scenes.get (i), _numFixations);
            positionsSeen += 1;
            if (positionsSeen % stepSize == 0) {
              result = new Pair (positionsSeen, _model.getTotalLtmNodes ());
              publish (result);
              setProgress (100 * _model.getTotalLtmNodes () / _maxSize);
              results.add (result);
            }
          }
          cycle += 1;
        }
        _model.constructTemplates ();
        
        result = new Pair (positionsSeen, _model.getTotalLtmNodes ());
        results.add (result);
        publish (result);

        return results;
      }

    @Override
      protected void process (List<Pair> results) {
        for (Pair pair : results) {
          _trainingTimes.add (pair.getFirst (), pair.getSecond ());
        }
      }

    protected void done () {
      _feedback.setValue (100);
      _stopAction.setEnabled (false);
      _trainAction.setEnabled (true);
      _model.unfreeze ();
    }
  }

  private class TrainAction extends AbstractAction implements ActionListener {

    public TrainAction () {
      super ("Train");
    }

    private int getNumFixations () {
      return ((SpinnerNumberModel)(_numFixations.getModel())).getNumber().intValue ();
    }

    private int getMaxCycles () {
      return ((SpinnerNumberModel)(_maxTrainingCycles.getModel())).getNumber().intValue ();
    }

    private int getMaxNetworkSize () {
      return ((SpinnerNumberModel)(_maxNetworkSize.getModel())).getNumber().intValue ();
    }

    public void actionPerformed (ActionEvent e) {
      _task = new TrainingThread (_model, _scenes, getMaxCycles (), getMaxNetworkSize (), getNumFixations ());
      _task.addPropertyChangeListener(
          new java.beans.PropertyChangeListener() {
            public  void propertyChange(java.beans.PropertyChangeEvent evt) {
              if ("progress".equals(evt.getPropertyName())) {
                _feedback.setValue ((Integer)evt.getNewValue());
              }
            }
          });

      _trainingTimes.clear (); // make sure we start graph afresh

      _feedback.setValue (0);
      _trainAction.setEnabled (false);
      _stopAction.setEnabled (true);

      _task.execute ();
    }
  }

  private TrainingThread _task;
  private JProgressBar _feedback;
  private TrainAction _trainAction;
  private StopAction _stopAction;

  private class StopAction extends AbstractAction implements ActionListener {
    public StopAction () {
      super ("Stop");
    }
    public void actionPerformed (ActionEvent e) {
      _task.cancel (true);
      _trainAction.setEnabled (true);
      _stopAction.setEnabled (false);
    }
  }

  private JLabel _recallSceneLabel;
  private SceneDisplay _recalledSceneDisplay;

  // -- set up the recall panel
  private JPanel recallPanel () {
    JPanel panel = new JPanel ();
    panel.setLayout (new GridLayout (1, 1));

    JSplitPane sp = new JSplitPane (
        JSplitPane.VERTICAL_SPLIT, 
        recallSetupPanel (),
        recallResultsPanel ());

    panel.add (sp);

    return panel;
  }

  private JPanel recallSetupPanel () {
    JPanel panel = new JPanel ();
    panel.setLayout (new BorderLayout ());

    panel.add (constructSelector (), BorderLayout.NORTH);
    panel.add (_sceneDisplay);
    panel.add (constructButtons (), BorderLayout.EAST);

    return panel;
  }

  private JLabel _precision, _recall, _omission, _commission;
  private JPanel recallResultsPanel () {
    _recallSceneLabel = new JLabel ("RECALLED SCENE");
    _recalledSceneDisplay = new SceneDisplay (new Scene ("empty", 
          _scenes.get(0).getHeight (), 
          _scenes.get(0).getWidth ()));
    _precision = new JLabel ("");
    _recall = new JLabel ("");
    _omission = new JLabel ("");
    _commission = new JLabel ("");

    JPanel panel = new JPanel ();
    panel.setLayout (new BorderLayout ());

    JPanel statistics = new JPanel ();
    statistics.setLayout (new GridLayout (4, 2));
    statistics.add (new JLabel ("Precision: ", SwingConstants.RIGHT));
    statistics.add (_precision);
    statistics.add (new JLabel ("Recall: ", SwingConstants.RIGHT));
    statistics.add (_recall);
    statistics.add (new JLabel ("Errors of omission: ", SwingConstants.RIGHT));
    statistics.add (_omission);
    statistics.add (new JLabel ("Errors of commission: ", SwingConstants.RIGHT));
    statistics.add (_commission);

    // TODO: there must be a better solution to stop the statistics panel spreading out
    JPanel statisticsPanel = new JPanel ();
    statisticsPanel.setLayout (new BorderLayout ());
    statisticsPanel.add (statistics, BorderLayout.NORTH);

    panel.add (_recallSceneLabel, BorderLayout.NORTH);
    panel.add (_recalledSceneDisplay);
    panel.add (statisticsPanel, BorderLayout.EAST);

    return panel;
  }

  private JComboBox _domainSelector;
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

  class AnalyseAction extends AbstractAction implements ActionListener {
    private JSpinner _numFixations;

    public AnalyseAction (JSpinner numFixations) {
      super ("Analyse Scenes");

      _numFixations = numFixations;
    }

    public void actionPerformed (ActionEvent e) {
       _analysisTask = new AnalysisThread (_numFixations);
       _analysisTask.addPropertyChangeListener(
           new java.beans.PropertyChangeListener() {
             public void propertyChange(java.beans.PropertyChangeEvent evt) {
               if ("progress".equals(evt.getPropertyName())) {
                 _feedbackAnalysis.setValue ((Integer)evt.getNewValue());
               }
             }
           });
       _feedbackAnalysis.setValue (0);
       _analyseAction.setEnabled (false);
       _stopAnalysisAction.setEnabled (true);

       _analysisTask.execute ();
    }
  }

  private class AnalysisThread extends SwingWorker<Void, Void> {
    private Map<Integer, Integer> _recallFrequencies;
    private JSpinner _numFixations;

    public AnalysisThread (JSpinner numFixations) {
      _numFixations = numFixations;
    }

    @Override
      public Void doInBackground () {
        _recallFrequencies = new HashMap<Integer, Integer> ();

        // loop through each scene, doing recall
        for (int i = 0; i < _scenes.size () && !isCancelled (); i++) {
          Scene scene = _scenes.get (i);
          _model.scanScene (scene, ((SpinnerNumberModel)(_numFixations.getModel())).getNumber().intValue ());
          for (Node node : _model.getPerceiver().getRecognisedNodes ()) {
            int id = node.getReference ();
            if (_recallFrequencies.containsKey (id)) {
              _recallFrequencies.put (id, _recallFrequencies.get(id) + 1);
            } else {
              _recallFrequencies.put (id, 1);
            }
          }
          setProgress (100 * i / _scenes.size ());
        }
        return null;
      }

    protected void done () {
      // finally, show results in window
      for (Integer key : _recallFrequencies.keySet ()) {
        _analysisScreen.append ("" + key + ", " + _recallFrequencies.get(key) + "\n");
      }

      _feedbackAnalysis.setValue (100);
      _stopAnalysisAction.setEnabled (false);
      _analyseAction.setEnabled (true);
    }
  }

  class RecallAction extends AbstractAction implements ActionListener {
    private JSpinner _numFixations;

    public RecallAction (JSpinner numFixations) {
      super ("Recall Scene");

      _numFixations = numFixations;
    }

    public void actionPerformed (ActionEvent e) {
      Scene scene =  _scenes.get(_sceneSelector.getSelectedIndex ());
      Scene recalledScene = _model.scanScene (scene, ((SpinnerNumberModel)(_numFixations.getModel())).getNumber().intValue ());
      _recallSceneLabel.setText (recalledScene.getName ());
      _recalledSceneDisplay.updateScene (recalledScene);
      _precision.setText ("" + scene.computePrecision (recalledScene));
      _recall.setText ("" + scene.computeRecall (recalledScene));
      _omission.setText ("" + scene.computeErrorsOfOmission (recalledScene));
      _commission.setText ("" + scene.computeErrorsOfCommission (recalledScene));
      _sceneDisplay.setFixations (_model.getPerceiver().getFixations ());
      // log results
      addLog ("\n" + recalledScene.getName ());
      addLog ("Fixations: ");
      for (Fixation fixation : _model.getPerceiver().getFixations ()) {
        addLog ("   " + fixation.toString ());
      }
      addLog ("Chunks used: ");
      for (Node node : _model.getVisualStm()) {
        addLog ("   " + "Node: " + node.getReference() + " " + node.getImage().toString ());
        if (_model.getCreateTemplates() && node.isTemplate ()) {
          addLog ("     Template:");
          addLog ("        filled item slots: ");
          for (ItemSquarePattern isp : node.getFilledItemSlots ()) {
            addLog ("         " + isp.toString ());
          }
          addLog ("        filled position slots: ");
          for (ItemSquarePattern isp : node.getFilledPositionSlots ()) {
            addLog ("         " + isp.toString ());
          }

        }
      }
      addLog ("Performance: ");
      addLog ("   Precision: " + scene.computePrecision (recalledScene));
      addLog ("   Recall: " + scene.computeRecall (recalledScene));
      addLog ("   Errors of Omission: " + scene.computeErrorsOfOmission (recalledScene));
      addLog ("   Errors of Commission: " + scene.computeErrorsOfCommission (recalledScene));
    }
  }

  private JPanel constructButtons () {

    Box buttons = Box.createVerticalBox ();
    JSpinner numFixations = new JSpinner (new SpinnerNumberModel (20, 1, 100, 1));
    
    JPanel labelledSpinner = new JPanel ();
    labelledSpinner.setLayout (new GridLayout (1, 2));
    labelledSpinner.add (new JLabel ("Number of fixations: "));
    labelledSpinner.add (numFixations);

    JButton recallButton = new JButton (new RecallAction (numFixations));
    recallButton.setToolTipText ("Scan shown scene and display results");

    final JCheckBox showFixations = new JCheckBox ("Show fixations", false);
    showFixations.setToolTipText ("Show fixations for recalled scene");
    showFixations.addActionListener (new ActionListener () {
      public void actionPerformed (ActionEvent e) {
        _sceneDisplay.setShowFixations (showFixations.isSelected ());
      }
    });

    buttons.add (Box.createRigidArea (new Dimension (0, 20)));
    buttons.add (labelledSpinner);
    buttons.add (recallButton);
    buttons.add (Box.createRigidArea (new Dimension (0, 20)));
    buttons.add (showFixations);

    // TODO: There must be a better solution to this problem!
    JPanel panel = new JPanel ();
    panel.setLayout (new BorderLayout ());
    panel.add (buttons, BorderLayout.NORTH);

    return panel;
  }

  // -- setup the log display
  private JTextArea _logScreen;

  private JPanel logPanel () {
    _logScreen = new JTextArea ();
    JPanel panel = new JPanel ();
    panel.setLayout (new BorderLayout ());
    panel.add (new JScrollPane (_logScreen));

    Box buttons = Box.createHorizontalBox ();
    buttons.add (new JButton (new SaveAction (_logScreen)));
    buttons.add (new JButton (new ClearAction (_logScreen)));
    panel.add (buttons, BorderLayout.SOUTH);

    return panel;
  }

  private class SaveAction extends AbstractAction implements ActionListener {
    private JTextArea _text;
    public SaveAction (JTextArea text) {
      super ("Save");
      _text = text;
    }
    public void actionPerformed (ActionEvent e) {
      File file = FileUtilities.getSaveFilename (_text);
      if (file != null) {
        try {
          FileWriter fw = new FileWriter (file);
          _text.write (fw);
          fw.close ();
        } catch (IOException ioe) {
          ; // ignore any problems
        }
      }
    }
  }

  private class ClearAction extends AbstractAction implements ActionListener {
    private JTextArea _text;
    public ClearAction (JTextArea text) {
      super ("Clear");
      _text = text;
    }
    public void actionPerformed (ActionEvent e) {
      _text.setText ("");
    }
  }

  private void addLog (String string) {
    _logScreen.append (string + "\n");
  }

  // -- setup the analyse display
  private JTextArea _analysisScreen;

  private JPanel analysePanel () {
    _analysisScreen = new JTextArea ();

     Box buttons = Box.createVerticalBox ();

    JSpinner numFixations = new JSpinner (new SpinnerNumberModel (20, 1, 100, 1));
    
    JPanel labelledSpinner = new JPanel ();
    labelledSpinner.setLayout (new GridLayout (1, 2));
    labelledSpinner.add (new JLabel ("Number of fixations: ", SwingConstants.RIGHT));
    labelledSpinner.add (numFixations);

//    JButton runAnalysis = new JButton (new AnalyseAction (numFixations));
//    runAnalysis.setToolTipText ("Scan all scenes, and record the frequencies of retrieved nodes");

    buttons.add (labelledSpinner);
    buttons.add (runAnalysisButtons (numFixations));

    // main panel
    JPanel panel = new JPanel ();
    panel.setLayout (new BorderLayout ());

    panel.add (buttons, BorderLayout.NORTH);

    panel.add (new JScrollPane (_analysisScreen));

    Box displayButtons = Box.createHorizontalBox ();
    displayButtons.add (new JButton (new SaveAction (_analysisScreen)));
    displayButtons.add (new JButton (new ClearAction (_analysisScreen)));
    panel.add (displayButtons, BorderLayout.SOUTH);

    return panel;
  }

  private AnalysisThread _analysisTask;
  private AnalyseAction _analyseAction;
  private StopAnalysisAction _stopAnalysisAction;
  private JProgressBar _feedbackAnalysis;

  private JPanel runAnalysisButtons (JSpinner numFixations) {
    JPanel panel = new JPanel ();

    _analyseAction = new AnalyseAction (numFixations);
    JButton analyseButton = new JButton (_analyseAction);
    analyseButton.setToolTipText ("Analyse CHREST model on all scenes");
    _stopAnalysisAction = new StopAnalysisAction ();
    _stopAnalysisAction.setEnabled (false);
    JButton stopAnalysisButton = new JButton (_stopAnalysisAction);
    stopAnalysisButton.setToolTipText ("Stop the current analysis");
    _feedbackAnalysis = new JProgressBar (0, 100);
    _feedbackAnalysis.setToolTipText ("Proportion of scenes analysed");
    _feedbackAnalysis.setValue (0);
    _feedbackAnalysis.setStringPainted (true);
    panel.add (analyseButton);
    panel.add (stopAnalysisButton);
    panel.add (_feedbackAnalysis);

    return panel;
  }

  private class StopAnalysisAction extends AbstractAction implements ActionListener {
    public StopAnalysisAction () {
      super ("Stop");
    }
    public void actionPerformed (ActionEvent e) {
      _analysisTask.cancel (true);
      _analyseAction.setEnabled (true);
      _stopAnalysisAction.setEnabled (false);
    }
  }
}

class SceneDisplay extends JPanel {
  private Scene _scene;
  private SceneView _sceneView;
  private List<Fixation> _fixations;
  private boolean _showFixations;

  public SceneDisplay (Scene scene) {
    super ();

    setLayout (new GridLayout (1, 1));
    _scene = scene;
    _sceneView = new SceneView ();
    _fixations = new ArrayList<Fixation> ();
    _showFixations = false;
    add (new JScrollPane (_sceneView));
  }

  public void updateScene (Scene scene) {
    _scene = scene;
    _sceneView.update ();
  }

  public void setFixations (List<Fixation> fixations) {
    _fixations = fixations;
    _sceneView.update ();
  }

  public void setShowFixations (boolean showFixations) {
    _showFixations = showFixations;
    _sceneView.update ();
  }

  class SceneView extends JPanel {
    private int _maxX, _maxY;

    SceneView () {
      super ();

      setBackground (Color.WHITE);
      update ();
    }

    private int offsetX = 10;
    private int offsetY = 10;
    private int scale = 20;

    public void update () {

      _maxX = scale * (_scene.getWidth () + 2); // + 2 for row heading and gaps
      _maxY = scale * (_scene.getHeight () + 2); // + 2 for column heading and gaps

      setPreferredSize (new Dimension (_maxX, _maxY));
      if (getParent () != null) ((JComponent)getParent ()).updateUI ();
      repaint ();
    }

    public void paint (Graphics g) {
      ((Graphics2D)g).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      super.paint (g); // make sure the background of the JPanel is drawn
      Graphics2D g2 = (Graphics2D)g;
      int fov = 2; // TODO ???

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

      // draw fixations
      int prevX = -1;
      int prevY = -1;
      if (_showFixations) {
        for (Fixation fixation : _fixations) {
          g2.setColor (Color.BLUE);
          g2.setStroke (new BasicStroke (2));
          int nextX = offsetX + scale * fixation.getX () + 5;
          int nextY = offsetY + scale * fixation.getY () + 5;
          if (prevX == -1 && prevY == -1) {
            ; // draw nothing for first oval
          } else {
            g2.drawLine (prevX, prevY, nextX+5, nextY+5);
          }
          g2.drawOval (nextX, nextY, scale-10, scale-10); 
          prevX = nextX+5; 
          prevY = nextY+5;
        }
      }
    }
  }
}
