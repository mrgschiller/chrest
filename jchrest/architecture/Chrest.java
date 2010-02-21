package jchrest.architecture;

import jchrest.lib.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
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

  /** 
   * Return a count of the number of nodes in visual long-term memory.
   */
  public int ltmVisualSize () {
    return _visualLtm.size ();
  }

  /**
   * Return the average depth of nodes in visual long-term memory.
   */
  public double getVisualLtmAverageDepth () {
    return _visualLtm.averageDepth ();
  }

  /**
   * Return a count of the number of nodes in verbal long-term memory.
   */
  public int ltmVerbalSize () {
    return _verbalLtm.size ();
  }

  /**
   * Return the average depth of nodes in verbal long-term memory.
   */
  public double getVerbalLtmAverageDepth () {
    return _verbalLtm.averageDepth ();
  }

  /**
   * Return a count of the number of nodes in action long-term memory.
   */
  public int ltmActionSize () {
    return _actionLtm.size ();
  }

  /**
   * Return the average depth of nodes in action long-term memory.
   */
  public double getActionLtmAverageDepth () {
    return _actionLtm.averageDepth ();
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

  /**
   * Write a description of the CHREST model to the given Writer object.
   */
  public void writeModel (Writer writer) throws IOException {
    FileUtilities.writeOpenTag (writer, "chrest");
    FileUtilities.writeTaggedInt (writer, "clock", _clock);
    FileUtilities.writeTaggedInt (writer, "add-link-time", _addLinkTime);
    FileUtilities.writeTaggedInt (writer, "discrimination-time", _discriminationTime);
    FileUtilities.writeTaggedInt (writer, "familiarisation-time", _familiarisationTime);
    FileUtilities.writeTaggedFloat (writer, "rho", _rho);
    FileUtilities.writeTaggedInt (writer, "field-of-view", _perceiver.getFieldOfView ());
    FileUtilities.writeTaggedInt (writer, "visual-stm-size", _visualStm.getSize ());
    FileUtilities.writeTaggedInt (writer, "verbal-stm-size", _verbalStm.getSize ());
    FileUtilities.writeTaggedInt (writer, "action-stm-size", _actionStm.getSize ());
    // write each ltm in turn
    FileUtilities.writeOpenTag (writer, "visual-ltm");
    _visualLtm.writeNode (writer);
    FileUtilities.writeCloseTag (writer, "visual-ltm");
    FileUtilities.writeNewLine (writer);
    FileUtilities.writeOpenTag (writer, "verbal-ltm");
    _verbalLtm.writeNode (writer);
    FileUtilities.writeCloseTag (writer, "verbal-ltm");
    FileUtilities.writeNewLine (writer);
    FileUtilities.writeOpenTag (writer, "action-ltm");
    _actionLtm.writeNode (writer);
    FileUtilities.writeCloseTag (writer, "action-ltm");
    FileUtilities.writeNewLine (writer);

    FileUtilities.writeCloseTag (writer, "chrest");
  }

  /**
   * Read a description of a Chrest model from the given buffered-reader, and construct 
   * a new instance of Chrest.
   */
  public static Chrest readFromFile (BufferedReader reader) throws ParsingErrorException {
    int clock, addLinkTime, familiarisationTime, discriminationTime, 
        visualStmSize, verbalStmSize, actionStmSize, fieldOfView;
    float rho;
    Node visualLtm, verbalLtm, actionLtm;

    FileUtilities.acceptOpenTag (reader, "chrest");
    while (!FileUtilities.checkCloseTag (reader, "chrest")) {
      if (FileUtilities.checkOpenTag (reader, "clock")) {
        clock = FileUtilities.readIntInTag (reader, "clock");
      } else if (FileUtilities.checkOpenTag (reader, "add-link-time")) {
        addLinkTime = FileUtilities.readIntInTag (reader, "add-link-time");
      } else if (FileUtilities.checkOpenTag (reader, "discrimination-time")) {
        discriminationTime = FileUtilities.readIntInTag (reader, "discrimination-time");
      } else if (FileUtilities.checkOpenTag (reader, "familiarisation-time")) {
        familiarisationTime = FileUtilities.readIntInTag (reader, "familiarisation-time");
      } else if (FileUtilities.checkOpenTag (reader, "rho")) {
        rho = FileUtilities.readFloatInTag (reader, "rho");
      } else if (FileUtilities.checkOpenTag (reader, "field-of-view")) {
        fieldOfView = FileUtilities.readIntInTag (reader, "field-of-view");
      } else if (FileUtilities.checkOpenTag (reader, "visual-stm-size")) {
        visualStmSize = FileUtilities.readIntInTag (reader, "visual-stm-size");
      } else if (FileUtilities.checkOpenTag (reader, "verbal-stm-size")) {
        verbalStmSize = FileUtilities.readIntInTag (reader, "verbal-stm-size");
      } else if (FileUtilities.checkOpenTag (reader, "action-stm-size")) {
        actionStmSize = FileUtilities.readIntInTag (reader, "action-stm-size");
      } else if (FileUtilities.checkOpenTag (reader, "visual-ltm")) {
        FileUtilities.acceptOpenTag (reader, "visual-ltm");
        visualLtm = readNodeFromFile (reader, "visual-ltm");
        FileUtilities.acceptCloseTag (reader, "visual-ltm");
      } else if (FileUtilities.checkOpenTag (reader, "verbal-ltm")) {
        FileUtilities.acceptOpenTag (reader, "verbal-ltm");
        verbalLtm = readNodeFromFile (reader, "verbal-ltm");
        FileUtilities.acceptCloseTag (reader, "verbal-ltm");
      } else if (FileUtilities.checkOpenTag (reader, "verbal-ltm")) {
        FileUtilities.acceptOpenTag (reader, "action-ltm");
        actionLtm = readNodeFromFile (reader, "action-ltm");
        FileUtilities.acceptCloseTag (reader, "action-ltm");
      } else { // no valid tag
        throw new ParsingErrorException ();
      }
    }
    FileUtilities.acceptCloseTag (reader, "chrest");

    return new Chrest ();
  }

  private static class ReadNode {

    static ReadNode readNodeFromFile (BufferedReader reader) throws ParsingErrorException {
      int reference, namedBy, followedBy;
      ListPattern contents, image;

      FileUtilities.acceptOpenTag (reader, "node");
      while (!FileUtilities.checkCloseTag (reader, "node")) {
        if (FileUtilities.checkOpenTag (reader, "reference")) {
          reference = FileUtilities.readIntInTag (reader, "reference");
        } else if (FileUtilities.checkOpenTag (reader, "contents")) {
          contents = ListPattern.readPattern (reader);
        } else if (FileUtilities.checkOpenTag (reader, "image")) {
          image = ListPattern.readPattern (reader);
        } else if (FileUtilities.checkOpenTag (reader, "children")) {

        } else if (FileUtilities.checkOpenTag (reader, "named-by")) {
          FileUtilities.acceptOpenTag (reader, "named-by");
          namedBy = FileUtilities.readIntInTag (reader, "reference");
          FileUtilities.acceptCloseTag (reader, "named-by");
        } else if (FileUtilities.checkOpenTag (reader, "followed-by")) {
          FileUtilities.acceptOpenTag (reader, "followed-by");
          followedBy = FileUtilities.readIntInTag (reader, "reference");
          FileUtilities.acceptCloseTag (reader, "followed-by");
        } else {
          throw new ParsingErrorException ();
        }
      }
      FileUtilities.acceptCloseTag (reader, "node");

      return new ReadNode ();
    }
  }

  /** 
   * Read all the nodes up to the close tag.  Construct a Chrest Node with correct object references.
   */
  private static Node readNodeFromFile (BufferedReader reader, String closeTag) throws ParsingErrorException {
    // 1. Read in all the nodes
    List<ReadNode> readNodes = new ArrayList<ReadNode> ();
    while (!FileUtilities.checkCloseTag (reader, closeTag)) {
      ReadNode newNode = ReadNode.readNodeFromFile (reader);
    }
    // 2. Convert into Chrest Nodes
    
    // 3. Add in link references
    
    // 4. Return the root node, which should have reference 0
    return new Node (null);
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

          // look at square given by first test link
          // then look to see if a test link has the same square and observed piece
          for (Link link : hypothesisChildren) {
            if (link.getTest().size () == 1) {
              if (link.getTest().getItem (0) instanceof ItemSquarePattern) {
                ItemSquarePattern testIos = (ItemSquarePattern)link.getTest().getItem (0);
                // check all details of test are correct
                if (testIos.getColumn () == _fixationX && 
                    testIos.getRow () == _fixationY &&
                    testIos.getItem().equals (_currentScene.getItem (_fixationY, _fixationX))) {
                  _visualStm.replaceHypothesis (link.getChildNode ());
                  _lastHeuristic = 1;
                  return true;
                    }
              }
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

