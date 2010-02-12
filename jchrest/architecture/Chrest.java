package jchrest.architecture;

import jchrest.lib.*;

import java.util.List;
import java.util.Observable;

/**
 * The parent class for an instance of a Chrest model.
 *
 * @author Peter C. R. Lane
 */
public class Chrest extends Observable {

  // internal clock
  private int _clock;  
  // timing parameters
  private int _addLinkTime;
  private int _discriminationTime;
  private int _familiarisationTime;
  // rho is the probability that a given learning operation will occur
  private float _rho;
  // long-term-memory holds information within the model permanently
  private Node _visualLtm;
  private Node _verbalLtm;
  private Node _actionLtm;
  // short-term-memory holds information within the model temporarily, usually within one experiment
  private Stm _visualStm;
  private Stm _verbalStm;
  private Stm _actionStm; // TODO: Incorporate into displays
  // Perception module
  private Perceiver _perceiver;

  public Chrest () {
    _addLinkTime = 10000;
    _discriminationTime = 10000;
    _familiarisationTime = 2000;
    _rho = 1.0f;

    _clock = 0;
    _visualLtm = new Node (Pattern.makeVisualList (new String[]{"Root"}));
    _verbalLtm = new Node (Pattern.makeVerbalList (new String[]{"Root"}));
    _actionLtm = new Node (Pattern.makeActionList (new String[]{"Root"}));
    _visualStm = new Stm (4);
    _verbalStm = new Stm (2);
    _actionStm = new Stm (4);

    _perceiver = new Perceiver ();
  }

  /**
   * Accessor to retrieve time to add a new link.
   */
  public int getAddLinkTime () {
    return _addLinkTime;
  }

  /**
   * Modify time to add a new link.
   */
  public void setAddLinkTime (int time) {
    _addLinkTime = time;
  }

  /**
   * Accessor to retrieve time to discriminate a new node.
   */
  public int getDiscriminationTime () {
    return _discriminationTime;
  }

  /**
   * Modify time to discriminate a new node.
   */
  public void setDiscriminationTime (int time) {
    _discriminationTime = time;
  }

  /**
   * Accessor to retrieve time to familiarise image of a node.
   */
  public int getFamiliarisationTime () {
    return _familiarisationTime;
  }

  /**
   * Modify time to familiarise image of a node.
   */
  public void setFamiliarisationTime (int time) {
    _familiarisationTime = time;
  }

  /**
   * Accessor to retrieve value of rho, the probability of learning an item.
   */
  public float getRho () {
    return _rho;
  }

  /**
   * Modify value of rho, the probability of learning an item.
   */
  public void setRho (float rho) {
    _rho = rho;
  }

  /**
   * Accessor to retrieve the size of visual short-term memory.
   */
  public int getVisualStmSize () {
    return _visualStm.getSize ();
  }

  /**
   * Modify size of visual short-term memory.
   */
  public void setVisualStmSize (int size) {
    _visualStm.setSize (size);
    setChanged ();
    notifyObservers ();
  }

  /**
   * Accessor to retrieve the size of verbal short-term memory.
   */
  public int getVerbalStmSize () {
    return _verbalStm.getSize ();
  }

  /**
   * Modify size of verbal short-term memory.
   */
  public void setVerbalStmSize (int size) {
    _verbalStm.setSize (size);
    setChanged ();
    notifyObservers ();
  }

  /**
   * Accessor to retrieve current time of model.
   */
  public int getClock () {
    return _clock;
  }

  /**
   * Advance the clock by given amount.
   */
  public void advanceClock (int time) {
    _clock += time;
    setChanged ();
  }

  /**
   * Accessor to retrieve visual short-term memory of model.
   */
  public Stm getVisualStm () {
    return _visualStm;
  }

  /**
   * Accessor to retrieve verbal short-term memory of model.
   */
  public Stm getVerbalStm () {
    return _verbalStm;
  }

  /**
   * Accessor to retrieve long-term memory of model.
   */
  public Node getLtm () {
    return _visualLtm;
  }

  /** 
   * Return a count of the number of nodes in long-term memory.
   */
  public int ltmSize () {
    return _visualLtm.size () + _verbalLtm.size () + _actionLtm.size ();
  }

  public Node getLtmByModality (ListPattern pattern) {
    if (pattern.isVisual ()) {
      return _visualLtm;
    } else if (pattern.isVerbal ()) {
      return _verbalLtm;
    } else { // if (pattern.isAction ()) 
      return _actionLtm;
    }
  }

  private Stm getStmByModality (ListPattern pattern) {
    if (pattern.isVisual ()) {
      return _visualStm;
    } else if (pattern.isVerbal ()) {
      return _verbalStm;
    } else { // if (pattern.isAction ()) 
      return _actionStm;
    }
  }

  private void addToStm (Node node) {
    getStmByModality(node.getImage()).add (node);
  }

  /**
   * Accessor to retrieve the model's perceiver object.
   */
  public Perceiver getPerceiver () {
    return _perceiver;
  }

  /** 
   * Retrieve a node in long-term memory using the given ListPattern.
   * The sorting process works through the children of the currentNode.
   * If the link's test matches the remaining part of the pattern, then 
   * the current node is updated, and searching continues through the 
   * children of the new node.
   */
  public Node recognise (ListPattern pattern) {
    Node currentNode = getLtmByModality (pattern);
    List<Link> children = currentNode.getChildren ();
    ListPattern sortedPattern = pattern;
    int nextLink = 0;

    while (nextLink < children.size ()) {
      Link link = children.get (nextLink);
      if (link.passes (sortedPattern)) {
        // update the current node and list of children
        currentNode = link.getChildNode ();
        children = link.getChildNode ().getChildren ();
        // remove the matched test from the sorted pattern
        sortedPattern = sortedPattern.remove (link.getTest ());
        nextLink = 0;
      } else { 
        // move on to the next link
        nextLink += 1;
      }
    }

    addToStm (currentNode);

    setChanged ();
    notifyObservers ();

    return currentNode;
  }

  /** 
   * Use given ListPattern to perform a step of learning within the network.
   * First, the pattern is sorted.  Then, if the retrieved node is the 
   * root node or its image mismatches the pattern, discrimination is 
   * used to extend the network.  Otherwise, new information will be added 
   * to the image using the pattern.
   */
  public Node recogniseAndLearn (ListPattern pattern, int time) {
    Node currentNode = recognise (pattern);
    if (_clock <= time) { // only try to learn if model clock is 'behind' the time of the call
      if (Math.random () < _rho) { // depending on _rho, may refuse to learn some random times
        _clock = time; // bring clock up to date
        if (!currentNode.getImage().equals (pattern)) { // only try any learning if image differs from pattern
          if (currentNode == getLtmByModality (pattern) || !currentNode.getImage().matches (pattern) || currentNode.getImage().isFinished ()) {
            currentNode = currentNode.discriminate (this, pattern);
            setChanged ();
          } else if (!currentNode.getImage().equals (pattern)) {
            currentNode = currentNode.familiarise (this, pattern);
            setChanged ();
          }
          addToStm (currentNode);
          notifyObservers ();
        }
      }
    }
    return currentNode;
  }

  public Node recogniseAndLearn (ListPattern pattern) {
    return recogniseAndLearn (pattern, _clock);
  }

  /**
   * Asks Chrest to return the image of the node obtained by sorting given 
   * pattern through the network.
   */
  public ListPattern recallPattern (ListPattern pattern) {
    return recognise(pattern).getImage ();
  }

  /** 
   * Asks Chrest to return the image of the node which follows the node 
   * obtained by sorting given pattern through the network.
   */
  public ListPattern followPattern (ListPattern pattern) {
    Node retrievedNode = recognise (pattern);
    if (retrievedNode.getFollowedBy () != null) {
      return retrievedNode.getFollowedBy().getImage ();
    } else {
      return null;
    }
  }

  /**
   * Asks Chrest to return the image of the node which names the node 
   * obtained by sorting given pattern through the network.
   */
  public ListPattern namePattern (ListPattern pattern) {
    Node retrievedNode = recognise (pattern);
    if (retrievedNode.getNamedBy () != null) {
      return retrievedNode.getNamedBy().getImage ();
    } else {
      return null;
    }
  }

  /**
   * Presents Chrest with a pair of patterns, which it should learn and 
   * then attempt to learn a link.
   */
  public void learnAndLinkPatterns (ListPattern pattern1, ListPattern pattern2, int time) {
    recogniseAndLearn (pattern1, time);
    recogniseAndLearn (pattern2, time);
    if (_clock <= time) {
      if (pattern1.isVisual ()) {
        _visualStm.learnLateralLinks (this);
      } else if (pattern1.isVerbal ()) {
        _verbalStm.learnLateralLinks (this);
      } else { // if (pattern1.isAction ())
        _actionStm.learnLateralLinks (this);
      }
      setChanged ();
      notifyObservers ();
    }
  }

  /**
   * Learns the two patterns assuming the time of presentation is the current 
   * Chrest clock time.
   */
  public void learnAndLinkPatterns (ListPattern pattern1, ListPattern pattern2) {
    learnAndLinkPatterns (pattern1, pattern2, _clock);
  }

  /**
   * Learn and link a visual and verbal pattern with a naming link.
   */
  public void learnAndNamePatterns (ListPattern pattern1, ListPattern pattern2, int time) {
    recogniseAndLearn (pattern1, time);
    recogniseAndLearn (pattern2, time);
    if (_clock <= time) {
      if (pattern1.isVisual () && pattern2.isVerbal () && _visualStm.getCount () > 0 && _verbalStm.getCount () > 0) {
        _visualStm.getItem(0).setNamedBy (_verbalStm.getItem (0));
        advanceClock (getAddLinkTime ());
      }
      setChanged ();
      notifyObservers ();
    }
  }

  public void learnAndNamePatterns (ListPattern pattern1, ListPattern pattern2) {
    learnAndNamePatterns (pattern1, pattern2, _clock);
  }

  /** 
   * Clear the STM and LTM of the model.
   */
  public void clear () {
    _clock = 0;
    _visualLtm = new Node (Pattern.makeVisualList (new String[]{"Root"}));
    _verbalLtm = new Node (Pattern.makeVerbalList (new String[]{"Root"}));
    _actionLtm = new Node (Pattern.makeActionList (new String[]{"Root"}));
    _visualStm.clear ();
    _verbalStm.clear ();
    setChanged ();
    notifyObservers ();
  }

  public String getHeuristicDescription () {
    return _perceiver.getHeuristicDescription ();
  }

    private final static java.util.Random _random = new java.util.Random ();

  /**
   * Perceiver is an inner class as it contains many specific methods to itself, but 
   * also needs access to details of the current _ltm and _stm.
   */
  public class Perceiver {
    private int _fixationX, _fixationY, _fieldOfView;
    int _lastHeuristic;
    private Scene _currentScene;

    protected Perceiver () {
      _fixationX = 0;
      _fixationY = 0;
      _fieldOfView = 2;
      _lastHeuristic = 0;
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
    }

    /** 
     * Initial fixation point - the centre of the scene.
     */
    public void start () {
      _fixationX = _currentScene.getWidth () / 2;
      _fixationY = _currentScene.getHeight () / 2;
    }

    /**
     * Try to move eye using LTM heuristic, return true if:
     *   -- square suggested by first child yields a piece which 
     *      allows model to follow a test link.
     */
    private boolean ltmHeuristic () {
      if (_visualStm.getCount () >= 1) {
        List<Link> hypothesisChildren = _visualStm.getItem(0).getChildren ();
        if (hypothesisChildren.isEmpty ()) return false;
        ListPattern test = hypothesisChildren.get(0).getTest ();
        if (test.isEmpty ()) return false;
        Pattern first = test.getItem (0);
        if (first instanceof ItemSquarePattern) {
          ItemSquarePattern ios = (ItemSquarePattern)first;
          _fixationX = ios.getColumn ();
          _fixationY = ios.getRow (); 

          for (Link link : hypothesisChildren) {
            if (_currentScene.getItem (_fixationY, _fixationX).equals (link.getTest ())) {
              _visualStm.replaceHypothesis (link.getChildNode ());
              _lastHeuristic = 1;
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

      for (int i = 0; i < 10; ++i) {
        int xDisplacement = _random.nextInt (_fieldOfView * 2 + 1) - _fieldOfView;
        int yDisplacement = _random.nextInt (_fieldOfView * 2 + 1) - _fieldOfView;
        if (!_currentScene.isEmpty (_fixationY + yDisplacement, _fixationX + xDisplacement)) {
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
          (_fixationX + xDisplacement > _currentScene.getWidth ()) ||
          (_fixationY + yDisplacement > _currentScene.getHeight ())) {
        _fixationX += 1;
        if (_fixationX == _currentScene.getWidth ()) {
          _fixationY += 1;
          _fixationX = 0;
        }
        if (_fixationY == _currentScene.getHeight ()) {
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
    public void moveEyeAndLearn () {
      if (ltmHeuristic ()) return;
      if (!randomItemHeuristic()) randomPlaceHeuristic ();
      recogniseAndLearn (_currentScene.getItems (_fixationX, _fixationY, 2));
    }

    public void moveEye () {
      if (ltmHeuristic ()) return;
      if (!randomItemHeuristic ()) randomPlaceHeuristic ();
      recognise (_currentScene.getItems (_fixationX, _fixationY, 2));
    }

    public String getHeuristicDescription () {
      if (_lastHeuristic == 0)
        return "No heuristic";
      else if (_lastHeuristic == 1)
        return "LTM heuristic";
      else if (_lastHeuristic == 2)
        return "Random item heuristic";
      else // if (_lastHeuristic == 3)
        return "Random piece heuristic";
    }

  }
}

