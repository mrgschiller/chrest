// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

// TODO: 
// 1. use _fixations instead of separate _fixationsX ... fields  DONE
// 2. include 'initial 4' eye fixations heuristics               DONE
// 3. Make fixation heuristics 'like' table 8.1 in book          DONE
//    (except forces 'random place' if others fail)
// 4. Include 'global strategies' for experienced model
// 5. Make learning of pattern be based on fixation sequence,    DONE - but not used as too slow
//    as with CHREST 2

package jchrest.architecture;

import jchrest.lib.Fixation;
import jchrest.lib.FixationType;
import jchrest.lib.ItemSquarePattern;
import jchrest.lib.ListPattern;
import jchrest.lib.Modality;
import jchrest.lib.Pattern;
import jchrest.lib.Scene;
import jchrest.lib.Square;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Perceiver class manages the model's interaction with an external, two-dimensional 
 * scene.
 */
public class Perceiver {
  private final static java.util.Random _random = new java.util.Random ();

  private final Chrest _model;
  private int _fixationX, _fixationY, _fieldOfView;
  FixationType _lastHeuristic;
  private Scene _currentScene;
  private List<Node> _recognisedNodes;

  protected Perceiver (Chrest model) {
    _model = model;
    _fixationX = 0;
    _fixationY = 0;
    _fieldOfView = 2;
    _lastHeuristic = FixationType.none;
    _fixations = new ArrayList<Fixation> ();
    _recognisedNodes = new ArrayList<Node> ();
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
  public void start (int targetNumberFixations) {
    _recognisedNodes.clear ();
    _targetNumberFixations = targetNumberFixations;

    _fixationX = _currentScene.getWidth () / 2;
    _fixationY = _currentScene.getHeight () / 2;
    _lastHeuristic = FixationType.start;
    addFixation (new Fixation (_lastHeuristic, _fixationX, _fixationY));
  }

  private boolean doInitialFixation () {
    Set<Square> squares = _model.getDomainSpecifics().proposeSalientSquareFixations (_currentScene, _model);
    if (squares.isEmpty ()) {
      return false;
    } else {
      Square square = (new ArrayList<Square>(squares)).get ((new java.util.Random()).nextInt (squares.size ()));
      addFixation (new Fixation (FixationType.salient, square.getColumn (), square.getRow ()));
      return true;
    }
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
            _lastHeuristic = FixationType.ltm;

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
        _lastHeuristic = FixationType.randomItem;

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

    _lastHeuristic = FixationType.randomPlace;

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
   */
  private void moveEyeUsingHeuristics () {
    double r = Math.random ();
    boolean fixationDone = false;
    if (r < 0.3333) { // try movement fixation
      List<Square> pieceMoves = _model.getDomainSpecifics().proposeMovementFixations (
          _currentScene, 
          new Square (_fixationY, _fixationX)
          );
      if (pieceMoves.size () > 0) { 
        int move = (new java.util.Random ()).nextInt (pieceMoves.size ());
        _fixationX = pieceMoves.get(move).getColumn ();
        _fixationY = pieceMoves.get(move).getRow ();
        _lastHeuristic = FixationType.proposedMove;
        fixationDone = true;
      }
    }
    if (r >= 0.3333 && r < 0.6667) { // try random item fixation
      fixationDone = randomItemHeuristic ();
    }
    if (!fixationDone) { // else try random place/global strategy
      if (_model.isExperienced ()) {
        // TODO: include global strategy
        randomPlaceHeuristic ();
      } else {
        randomPlaceHeuristic ();
      }
    }
    // randomPlace / globalStrategy guaranteed to succeed

    addFixation (new Fixation (_lastHeuristic, _fixationX, _fixationY));
  }

  /**
   * Find the next fixation point using one of the available 
   * heuristics, and then learn from the new pattern.
   */
  public void moveEyeAndLearn () {
    boolean fixationDone = false;
    if (doingInitialFixations ()) {
      fixationDone = doInitialFixation ();
    }
    if (!fixationDone) {
      fixationDone = ltmHeuristic ();
    }
    if (!fixationDone) {
      moveEyeUsingHeuristics ();
    }
    // learn pattern found from fixations
    // TODO: this is learning as used in CHREST 2
    // but here it produces learning too slow
    // Need to reconsider the learning during perception issue.
//    if (shouldLearnFixations ()) {
//      learnFixatedPattern ();
//    }

    // simplified version of learning, learns pattern at current point
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
    boolean fixationDone = false;
    if (doingInitialFixations ()) {
      fixationDone = doInitialFixation ();
      if (fixationDone) {
        node = _model.recognise (_model.getDomainSpecifics().normalise (_currentScene.getItems (_fixationX, _fixationY, 2)));
      }
    }
    if (!fixationDone) {
      fixationDone = ltmHeuristic ();
      if (fixationDone && _model.getVisualStm().getCount () >= 1) {
        node = _model.getVisualStm().getItem(0);
      }
    }
    if (!fixationDone) {
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

  List<Fixation> _fixations = new ArrayList<Fixation> ();
  private int _fixationsLearnFrom = 0; // used to mark first fixation to learn from
  private int _targetNumberFixations = 20; // used to store the number of fixations in a scene

  public void clearFixations () {
    _fixations.clear ();
    _fixationsLearnFrom = 0;
  }

  public List<Fixation> getFixations () {
    return _fixations;
  }

  private void addFixation (Fixation fixation) {
    _fixations.add (fixation);
  }

  // learn pattern found from fixations
  // -- in CHREST 2 this is triggered by:
  //    a. last fixation an empty square
  //    b. last fixation a result of random item or global strategy
  //    c. reached max number of fixations
  //    d. cycle in fixations
  private boolean shouldLearnFixations () {
    int lastFixationIndex = _fixations.size () - 1;
    if (lastFixationIndex <= _fixationsLearnFrom) { // nothing to learn
      return false;
    }
    Fixation lastFixation = _fixations.get (lastFixationIndex);
    // is last fixation to an empty square?
    if (_currentScene.isEmpty (lastFixation.getX (), lastFixation.getY ())) {
      return true;
    }
    // is fixation a global strategy?
    if (lastFixation.getType().equals (FixationType.global)) {
     return true;
    }
    // is fixation a random item?
    if (lastFixation.getType().equals (FixationType.randomItem)) {
      return true;
    }
    // reached limit of fixations?
    if (lastFixationIndex == _targetNumberFixations) {
      return true;
    }
    // does last fixation form a cycle?
    for (int i = 0; i < lastFixationIndex; ++i) {
      if (lastFixation.getX () == _fixations.get(i).getX () &&
          lastFixation.getY () == _fixations.get(i).getY ()) {
        return true;
      }
    }
    // otherwise, nothing to learn
    return false;
  }

  private void learnFixatedPattern () {
    ListPattern fixatedPattern = new ListPattern (Modality.VISUAL);
    for (int i = _fixationsLearnFrom; i < _fixations.size () - 1; ++i) {
      if (!_currentScene.isEmpty (_fixations.get(i).getX (), _fixations.get(i).getY ())) {
        fixatedPattern.add (new ItemSquarePattern (
              _currentScene.getItem (_fixations.get(i).getX (), _fixations.get(i).getY ()),
              _fixations.get(i).getY (),
              _fixations.get(i).getX ()
              ));
      }
    }
    
    System.out.println ("Perception Learning: " + _model.getDomainSpecifics().normalise (fixatedPattern));
    _model.recogniseAndLearn (_model.getDomainSpecifics().normalise (fixatedPattern));
    // begin cycle again, from point where we stopped
    _fixationsLearnFrom = _fixations.size () - 1;
  }

  // indicator for if the eye is processing the initial fixations
  // arbitrarily set to '4', from de Groot and Gobet (1996)
  private boolean doingInitialFixations () {
    return _fixations.size () <= 4;
  }

  public List<Node> getRecognisedNodes () {
    return _recognisedNodes;
  }
}

