package jchrest.architecture;

import jchrest.lib.*;

import java.util.ArrayList;
import java.util.List;

/**
 * Perceiver 
 */
public class Perceiver {
  private final static java.util.Random _random = new java.util.Random ();

  private Chrest _model;
  private int _fixationX, _fixationY, _fieldOfView;
  int _lastHeuristic;
  private Scene _currentScene;
  private List<Integer> _fixationsX, _fixationsY, _fixationsType;
  private List<Node> _recognisedNodes;

  protected Perceiver (Chrest model) {
    _model = model;
    _fixationX = 0;
    _fixationY = 0;
    _fieldOfView = 2;
    _lastHeuristic = 0;
    _fixationsX = new ArrayList<Integer> ();
    _fixationsY = new ArrayList<Integer> ();
    _fixationsType = new ArrayList<Integer> ();
    _fixations = new ArrayList<Fixation> ();
    _recognisedNodes = new ArrayList<Node> ();
  }

  public int getFixationX () {
    return _fixationX;
  }

  public void setFixationX (int x) {
    _fixationX = x;
  }

  public int getFixationY () {
    return _fixationY;
  }

  public void setFixationY (int y) {
    _fixationY = y;
  }

  public int getFieldOfView () {
    return _fieldOfView;
  }

  public void setFieldOfView (int fov) {
    _fieldOfView = fov;
  }

  public void setScene (Scene scene) {
    _currentScene = scene;
    clearFixations ();
  }

  /** 
   * Initial fixation point - the centre of the scene.
   */
  public void start () {
    _fixationsX.clear ();
    _fixationsY.clear ();
    _fixationsType.clear ();
    _recognisedNodes.clear ();

    _fixationX = _currentScene.getWidth () / 2;
    _fixationY = _currentScene.getHeight () / 2;
  }

  /**
   * Try to move eye using LTM heuristic, return true if:
   *   -- square suggested by first child yields a piece which 
   *      allows model to follow a test link.
   * Note: Chrest-2.1 has three further facilities
   *   1. does a call to discrimination first, to update STM
   *   2. checks that move from previous square to proposed square 
   *      has not been done before in this fixation cycle
   *   3. keeps track of how often this hypothesis node has been queried,
   *      so repeat queries will suggest alternate squares
   *      (i.e. proposed square does not lead to descending a link,
   *            so hypothesis not changed, and so next link in sequence will
   *            be tried.)
   */
  private boolean ltmHeuristic () {
    if (_model.getVisualStm().getCount () >= 1) {
      List<Link> hypothesisChildren = _model.getVisualStm().getItem(0).getChildren ();
      if (hypothesisChildren.isEmpty ()) return false;
      //        System.out.println ("Checking LTM heuristic");
      for (int i = 0; i < hypothesisChildren.size () && i < 1; ++i) { // *** i == 0 only
        ListPattern test = hypothesisChildren.get(i).getTest ();
        if (test.isEmpty ()) continue; // return false;
        Pattern first = test.getItem (0);
        //        System.out.println ("Checking: " + first);
        if (first instanceof ItemSquarePattern) {
          ItemSquarePattern ios = (ItemSquarePattern)first;

          // check if we should make the fixation
          // 1. is it a different square?
          if (ios.getColumn()-1 == _fixationX && 
              ios.getRow()-1 == _fixationY) {
            ; // return false; // we are already at this square
          } else {
            // all ok, so we make the fixation
            _fixationX = ios.getColumn ()-1; // because ios start from 1
            _fixationY = ios.getRow ()-1; 
            _lastHeuristic = 1;
            _fixationsX.add (_fixationX);
            _fixationsY.add (_fixationY);
            _fixationsType.add (_lastHeuristic);

            addFixation (new Fixation (_lastHeuristic, _fixationX, _fixationY));
            // look at square given by first test link
            // then look to see if a test link has the same square and observed piece
            for (Link link : hypothesisChildren) {
              if (link.getTest().size () == 1) {
                // Note: using first test created gives more uses of LTM heuristic
                if (link.getTest().getItem (link.getTest().size() - 1) instanceof ItemSquarePattern) {
                  ItemSquarePattern testIos = (ItemSquarePattern)link.getTest().getItem (0);
                  // check all details of test are correct
                  if (testIos.getColumn () - 1 == _fixationX && 
                      testIos.getRow () - 1 == _fixationY &&
                      testIos.getItem().equals (_currentScene.getItem (_fixationY, _fixationX))) {
                    _model.getVisualStm().replaceHypothesis (link.getChildNode ());
                      }
                }
              }
            }
            // return true, as we made the fixation
            return true;
          }
        }
      }
    }
    return false;
  }

  /**
   * Try to move eye to random item in periphery.
   */
  private boolean randomItemHeuristic () {

    for (int i = 0; i < 3; ++i) { // *** Parameter controls how likely 'item' over 'place'
      int xDisplacement = _random.nextInt (_fieldOfView * 2 + 1) - _fieldOfView;
      int yDisplacement = _random.nextInt (_fieldOfView * 2 + 1) - _fieldOfView;
      if (!_currentScene.isEmpty (_fixationY + yDisplacement, _fixationX + xDisplacement)
          && _fixationX < _currentScene.getWidth ()
          && _fixationY < _currentScene.getHeight ()) {
        _fixationX += xDisplacement;
        _fixationY += yDisplacement;
        _lastHeuristic = 2;

        return true;
          }
    }
    return false;
  }

  /**
   * Move eye to random position in periphery.
   */
  private void randomPlaceHeuristic () {
    int xDisplacement = _random.nextInt (_fieldOfView * 2 + 1) - _fieldOfView;
    int yDisplacement = _random.nextInt (_fieldOfView * 2 + 1) - _fieldOfView;

    _lastHeuristic = 3;

    if ((xDisplacement == 0 && yDisplacement == 0) || 
        (_fixationX + xDisplacement < 0) ||
        (_fixationY + yDisplacement < 0) ||
        (_fixationX + xDisplacement >= _currentScene.getWidth ()) ||
        (_fixationY + yDisplacement >= _currentScene.getHeight ())) {
      _fixationX += 1;
      // check legality of new fixation
      if (_fixationX >= _currentScene.getWidth ()) {
        _fixationY += 1;
        _fixationX = 0;
      }
      if (_fixationY >= _currentScene.getHeight ()) {
        _fixationX = 0;
        _fixationY = 0;
      }
    } else {
      _fixationX += xDisplacement;
      _fixationY += yDisplacement;
    }
  }

  /**
   * Find the next fixation point using one of the available 
   * heuristics.
   * TODO: Add in domain-specific heuristics
   * Also, global strategies for moving to a 'new' part of the scene.
   */
  private void moveEyeUsingHeuristics () {
    List<Square> pieceMoves = _model.getDomainSpecifics().proposeMovementFixations (_currentScene, 
        new Square (_fixationY, _fixationX));
    if (pieceMoves.size () > 0) { // && Math.random () < 0.5)  // TODO: Think about adding random miss of heuristic
      int move = (new java.util.Random ()).nextInt (pieceMoves.size ());
      _fixationX = pieceMoves.get(move).getColumn ();
      _fixationY = pieceMoves.get(move).getRow ();
      _lastHeuristic = 4;
    } else if (!randomItemHeuristic()) {
      randomPlaceHeuristic ();
    }

    _fixationsX.add (_fixationX);
    _fixationsY.add (_fixationY);
    _fixationsType.add (_lastHeuristic);
    addFixation (new Fixation (_lastHeuristic, _fixationX, _fixationY));
  }

  /**
   * Find the next fixation point using one of the available 
   * heuristics, and then learn from the new pattern.
   */
  public void moveEyeAndLearn () {
    if (ltmHeuristic ()) return;
    moveEyeUsingHeuristics ();
    _model.recogniseAndLearn (_model.getDomainSpecifics().normalise (_currentScene.getItems (_fixationX, _fixationY, 2)));
    // NB: template construction is only assumed to occur after training, so 
    // template completion code is not included here
  }

  /**
   * Find the next fixation point using one of the available 
   * heuristics, and simply move the eye to that point.
   */
  public void moveEye () {
    Node node = _model.getVisualLtm ();
    if (ltmHeuristic ()) {
      if (_model.getVisualStm().getCount () >= 1) {
        node = _model.getVisualStm().getItem(0);
      }
    } else {
      moveEyeUsingHeuristics ();
      node = _model.recognise (_model.getDomainSpecifics().normalise (_currentScene.getItems (_fixationX, _fixationY, 2)));
    }
    _recognisedNodes.add (node);
    // Attempt to fill out the slots on the top-node of visual STM with the currently 
    // fixated items
    if (_model.getVisualStm().getCount () >= 1) {
      _model.getVisualStm().getItem(0).fillSlots (_currentScene.getItems (_fixationX, _fixationY, 2));
    }
  }

  public String getHeuristicDescription () {
    if (_lastHeuristic == 0)
      return "No heuristic";
    else if (_lastHeuristic == 1)
      return "LTM heuristic";
    else if (_lastHeuristic == 2)
      return "Random item heuristic";
    else if (_lastHeuristic == 3)
      return "Random place heuristic";
    else // if (_lastHeuristic == 4)
      return "Follow proposed move heuristic";
  }

  public int getNumberFixations () {
    return _fixationsX.size ();
  }

  public int getFixationsX (int fixation) {
    return _fixationsX.get (fixation);
  }

  public int getFixationsY (int fixation) {
    return _fixationsY.get (fixation);
  }

  public int getFixationsType (int fixation) {
    return _fixationsType.get (fixation);
  }

  List<Fixation> _fixations = new ArrayList<Fixation> ();

  public void clearFixations () {
    _fixations.clear ();
  }

  public List<Fixation> getFixations () {
    return _fixations;
  }

  private void addFixation (Fixation fixation) {
    _fixations.add (fixation);
  }

  public List<Node> getRecognisedNodes () {
    return _recognisedNodes;
  }
}

