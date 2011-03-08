package jchrest.architecture;

import jchrest.lib.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Observable;

/**
 * The parent class for an instance of a Chrest model.
 *
 * @author Peter C. R. Lane
 */
public class Chrest extends Observable {
  // Domain definitions, if used
  DomainSpecifics _domainSpecifics;
  // internal clock
  private int _clock;  
  // timing parameters
  private int _addLinkTime;
  private int _discriminationTime;
  private int _familiarisationTime;
  // rho is the probability that a given learning operation will occur
  private float _rho;
  // parameter for construction of similarity link
  // - determines number of overlapping items in node images
  public static int SIMILARITY_THRESHOLD = 1;
  // template construction parameters
  private boolean _createTemplates;
  public static int MIN_LEVEL = 3;
  public static int MIN_OCCURRENCES = 2;
  // long-term-memory holds information within the model permanently
  private int _totalNodes;
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
    _domainSpecifics = new GenericDomain ();
    _addLinkTime = 10000;
    _discriminationTime = 10000;
    _familiarisationTime = 2000;
    _rho = 1.0f;

    _clock = 0;
    _totalNodes = 0;
    _visualLtm = new Node (this, 0, Pattern.makeVisualList (new String[]{"Root"}));
    _verbalLtm = new Node (this, 0, Pattern.makeVerbalList (new String[]{"Root"}));
    _actionLtm = new Node (this, 0, Pattern.makeActionList (new String[]{"Root"}));
    _totalNodes = 0; // Node constructor will have incremented _totalNodes, so reset to 0
    _visualStm = new Stm (4);
    _verbalStm = new Stm (2);
    _actionStm = new Stm (4);

    _createTemplates = true;
    _perceiver = new Perceiver ();
  }

  /**
   * Private constructor for use when reading a Chrest model from file.
   * Allows all parameters and data structures to be set.
   */
  private Chrest (int addLinkTime, int discriminationTime, int familiarisationTime, float rho,
      int clock, Node visualLtm, Node verbalLtm, Node actionLtm, 
      int visualStmSize, int verbalStmSize, int actionStmSize, 
      int fieldOfView) {
    _domainSpecifics = new GenericDomain ();
    _addLinkTime = addLinkTime;
    _discriminationTime = discriminationTime;
    _familiarisationTime = familiarisationTime;
    _rho = rho;
    _clock = clock;
    _totalNodes = 1000000; // TODO: This is broken
    _visualLtm = visualLtm;
    _verbalLtm = verbalLtm;
    _actionLtm = actionLtm;
    _visualStm = new Stm (visualStmSize);
    _verbalStm = new Stm (verbalStmSize);
    _actionStm = new Stm (actionStmSize);

    _perceiver = new Perceiver ();
    _perceiver.setFieldOfView (fieldOfView);
  }

  /**
   * Set the domain specification.
   */
  public void setDomain (DomainSpecifics domain) {
    _domainSpecifics = domain;
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
   * Modify option to create templates.
   */
  public void setCreateTemplates (boolean value) {
    _createTemplates = value;
  }

  /**
   * Accessor to option of whether to create templates.
   */
  public boolean getCreateTemplates () {
    return _createTemplates;
  }

  /**
   * Modify values for template construction.
   */
  public void setTemplateConstructionParameters (int minLevel, int minOccurrences) {
    MIN_LEVEL = minLevel;
    MIN_OCCURRENCES = minOccurrences;
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
   * Retrieve the next available node number.
   * Package access only, as should only be used by Node.java.
   */
  int getNextNodeNumber () {
    _totalNodes += 1;
    return _totalNodes;
  }

  /**
   * Accessor to retrieve the total number of nodes within LTM.
   */
  public int getTotalLtmNodes () {
    return _totalNodes;
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
   * Return the average image size of nodes in visual long-term memory.
   */
  public double getVisualLtmAverageImageSize () {
    return _visualLtm.averageImageSize ();
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

  /**
   * Construct templates.  Note, the template construction process only 
   * currently works for visual patterns using the ItemSquarePattern primitive.
   */
  public void constructTemplates () {
    if (_createTemplates) {
      _visualLtm.constructTemplates ();
    }
  }

  /**
   * Return a count of the number of templates in the model's visual LTM.
   */
  public int countTemplates () {
    return _visualLtm.countTemplates ();
  }

  /**
   * Return the root node of the long-term memory which the given pattern
   * would be sorted through, based on its modality.
   */
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

  /**
   * Add given node to STM.  Check for formation of similarity links by
   * comparing incoming node with the hypothesis, or 'largest', node.
   */
  private void addToStm (Node node) {
    Stm stm = getStmByModality (node.getImage ());

    if (stm.getCount () > 0) {
      Node check = stm.getItem (0);
      if (node.getImage().isSimilarTo (check.getImage (), SIMILARITY_THRESHOLD)) {
        node.addSimilarNode (check);
      }
    }

    // TODO: Check if this is the best place
    // Idea is that node's filled slots are cleared when put into STM, 
    // are filled whilst in STM, and forgotten when it leaves.
    node.clearFilledSlots (); 
    stm.add (node);
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

    // see if more informative node in similarity links
    // TODO: Explore if this should be used during training
    //       leads to small image size, but quickly produces big networks
//    for (Node similar : currentNode.getSimilarNodes ()) {
//      if (similar.information () > currentNode.information ()) {
//        currentNode = similar;
//      }
//    }

    // add to STM
    addToStm (currentNode);

    // inform observers of a change in model's state
    setChanged ();
    notifyObservers ();

    // return it
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
          } else if (!currentNode.getImage().equals (pattern)) {
            currentNode = currentNode.familiarise (this, pattern);
          }
          addToStm (currentNode);
          setChanged ();
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

  public void learnScene (Scene scene, int numFixations) {
    _perceiver.setScene (scene);
    _perceiver.start ();
    for (int i = 0; i < numFixations; i++) {
      _perceiver.moveEyeAndLearn ();
    }
  }

  /** 
   * Scan given scene, then return a scene which would be recalled.
   */
  public Scene scanScene (Scene scene, int numFixations) {
    // scan given scene, without learning
    _visualStm.clear ();
    _perceiver.setScene (scene);
    _perceiver.start ();
    for (int i = 0; i < numFixations; i++) {
      _perceiver.moveEye ();
    }
    // build up and return recalled scene
    Scene recalledScene = new Scene ("Recalled scene of " + scene.getName (), 
        scene.getHeight (), scene.getWidth ());
    // -- get items from image in STM, and optionally template slots
    // TODO: use frequency count in recall
    for (Node node : _visualStm.getContents ()) {
      ListPattern recalledInformation = node.getImage();
      if (_createTemplates) { // check if templates needed
        recalledInformation = recalledInformation.append(node.getFilledSlots ());
      }
      for (int i = 0; i < recalledInformation.size (); i++) {
        PrimitivePattern item = recalledInformation.getItem (i);
        if (item instanceof ItemSquarePattern) {
          ItemSquarePattern ios = (ItemSquarePattern)item;
          recalledScene.setItem (ios.getRow ()-1, ios.getColumn ()-1, ios.getItem ());
        }
      }
    }

    return recalledScene;
  }

  /** 
   * Clear the STM and LTM of the model.
   */
  public void clear () {
    _clock = 0;
    _visualLtm = new Node (this, 0, Pattern.makeVisualList (new String[]{"Root"}));
    _verbalLtm = new Node (this, 0, Pattern.makeVerbalList (new String[]{"Root"}));
    _actionLtm = new Node (this, 0, Pattern.makeActionList (new String[]{"Root"}));
    _totalNodes = 0;
    _visualStm.clear ();
    _verbalStm.clear ();
    setChanged ();
    notifyObservers ();
  }

  public String getHeuristicDescription () {
    return _perceiver.getHeuristicDescription ();
  }

  /** 
   * Write model to given Writer object in VNA format
   */
  public void writeModelAsVna (Writer writer) throws IOException {
    writer.write ("*Node data\n\"ID\", \"contents\"\n");
    _visualLtm.writeNodeAsVna (writer);
    writer.write ("*Tie data\nFROM TO\n");
    _visualLtm.writeLinksAsVna (writer);
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
   * TODO: This is currently broken.
   */
  public static Chrest readFromFile (BufferedReader reader) throws ParsingErrorException {
    int clock = -1;
    int addLinkTime = -1;
    int familiarisationTime = -1;
    int discriminationTime = -1; 
    int visualStmSize = -1;
    int verbalStmSize = -1;
    int actionStmSize = -1;
    int fieldOfView = -1;
    float rho = -1.0f;
    Node visualLtm = null;
    Node verbalLtm = null;
    Node actionLtm = null;
    // -- store a list of read nodes
    List<ReadNode> allReadNodes = new ArrayList<ReadNode> ();
    // -- store a map from node reference to the real Chrest node
    Map<Integer, Node> nodes = new HashMap<Integer, Node> ();
    Chrest model = new Chrest ();

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
        visualLtm = readNodeFromFile (model, allReadNodes, nodes, reader, "visual-ltm");
        FileUtilities.acceptCloseTag (reader, "visual-ltm");
      } else if (FileUtilities.checkOpenTag (reader, "verbal-ltm")) {
        FileUtilities.acceptOpenTag (reader, "verbal-ltm");
        verbalLtm = readNodeFromFile (model, allReadNodes, nodes, reader, "verbal-ltm");
        FileUtilities.acceptCloseTag (reader, "verbal-ltm");
      } else if (FileUtilities.checkOpenTag (reader, "action-ltm")) {
        FileUtilities.acceptOpenTag (reader, "action-ltm");
        actionLtm = readNodeFromFile (model, allReadNodes, nodes, reader, "action-ltm");
        FileUtilities.acceptCloseTag (reader, "action-ltm");
      } else { // no valid tag
        throw new ParsingErrorException ("Chrest: unknown tag");
      }
    }
    FileUtilities.acceptCloseTag (reader, "chrest");

    if (addLinkTime == -1 || discriminationTime == -1 || familiarisationTime == -1 || rho == -1.0f ||
        clock == -1 || visualLtm == null || verbalLtm == null || actionLtm == null || 
        visualStmSize == -1 || verbalStmSize == -1 || actionStmSize == -1 || fieldOfView == -1) {
      throw new ParsingErrorException ("Chrest: not completely defined");
        }

    // Add in lateral links
    for (ReadNode node : allReadNodes) {
      Node sourceNode = nodes.get (node.getReference ());

      if (node.hasNamedByLink ()) {
        sourceNode.setNamedBy (nodes.get (node.getNamedByReference ()));
      }

      if (node.hasFollowedByLink ()) {
        sourceNode.setFollowedBy (nodes.get (node.getFollowedByReference ()));
      }
    }

    return new Chrest (addLinkTime, discriminationTime, familiarisationTime, rho, 
        clock, visualLtm, verbalLtm, actionLtm, visualStmSize, verbalStmSize, actionStmSize,
        fieldOfView);
  }

  /**
   * Class to hold the information read in about a link.
   */
  private static class ReadLink {
    private ListPattern _test;
    private Integer _child;

    ReadLink (ListPattern test, Integer child) {
      _test = test;
      _child = child;
    }

    ListPattern getTest () {
      return _test;
    }

    Integer getChild () {
      return _child;
    }

    public static ReadLink readLinkFromFile (BufferedReader reader) throws ParsingErrorException {
      ListPattern test = null;
      Integer child = null;
      FileUtilities.acceptOpenTag (reader, "link");
      while (!FileUtilities.checkCloseTag (reader, "link")) {
        if (FileUtilities.checkOpenTag (reader, "child")) {
          child = FileUtilities.readIntInTag (reader, "child");
        } else if (FileUtilities.checkOpenTag (reader, "test")) {
          FileUtilities.acceptOpenTag (reader, "test");
          test = ListPattern.readPattern (reader);
          FileUtilities.acceptCloseTag (reader, "test");
        } else { // unknown tag
          throw new ParsingErrorException ("Link: unknown tag");
        }
      }

      FileUtilities.acceptCloseTag (reader, "link");

      // check everything has been initialised
      if (test == null || child == null) {
        throw new ParsingErrorException ("Link not complete");
      }

      return new ReadLink (test, child);
    }
  }

  /**
   * Class to hold information read in about a node.
   */
  private static class ReadNode {
    private int _reference;
    private ListPattern _contents;
    private ListPattern _image;
    private List<ReadLink> _children;
    private Integer _namedBy;
    private Integer _followedBy;

    ReadNode (int reference, ListPattern contents, ListPattern image, List<ReadLink> children, 
        Integer namedBy, Integer followedBy) {
      _reference = reference;
      _contents = contents;
      _image = image;
      _children = children;
      _namedBy = namedBy;
      _followedBy = followedBy;
    }

    Integer getReference () {
      return _reference;
    }

    ListPattern getContents () {
      return _contents;
    }

    ListPattern getImage () {
      return _image;
    }

    List<ReadLink> getChildren () {
      return _children;
    }

    boolean hasNamedByLink () {
      return _namedBy != null;
    }

    Integer getNamedByReference () {
      return _namedBy;
    }

    boolean hasFollowedByLink () {
      return _followedBy != null;
    }

    Integer getFollowedByReference () {
      return _followedBy;
    }

    static ReadNode readNodeFromFile (BufferedReader reader) throws ParsingErrorException {
      int reference = -1;
      Integer namedBy = null;
      Integer followedBy = null;
      ListPattern contents = null;
      ListPattern image = null;
      List<ReadLink> children = new ArrayList<ReadLink> ();

      FileUtilities.acceptOpenTag (reader, "node");
      while (!FileUtilities.checkCloseTag (reader, "node")) {
        if (FileUtilities.checkOpenTag (reader, "reference")) {
          reference = FileUtilities.readIntInTag (reader, "reference");
        } else if (FileUtilities.checkOpenTag (reader, "contents")) {
          FileUtilities.acceptOpenTag (reader, "contents");
          contents = ListPattern.readPattern (reader);
          FileUtilities.acceptCloseTag (reader, "contents");
        } else if (FileUtilities.checkOpenTag (reader, "image")) {
          FileUtilities.acceptOpenTag (reader, "image");
          image = ListPattern.readPattern (reader);
          FileUtilities.acceptCloseTag (reader, "image");
        } else if (FileUtilities.checkOpenTag (reader, "children")) {
          FileUtilities.acceptOpenTag (reader, "children");
          while (!FileUtilities.checkCloseTag (reader, "children")) {
            if (FileUtilities.checkOpenTag (reader, "link")) {
              children.add (ReadLink.readLinkFromFile (reader));
            } else { // unknown tag
              throw new ParsingErrorException ("Node children: unknown tag");
            }
          }
          FileUtilities.acceptCloseTag (reader, "children");
        } else if (FileUtilities.checkOpenTag (reader, "named-by")) {
          FileUtilities.acceptOpenTag (reader, "named-by");
          namedBy = FileUtilities.readIntInTag (reader, "reference");
          FileUtilities.acceptCloseTag (reader, "named-by");
        } else if (FileUtilities.checkOpenTag (reader, "followed-by")) {
          FileUtilities.acceptOpenTag (reader, "followed-by");
          followedBy = FileUtilities.readIntInTag (reader, "reference");
          FileUtilities.acceptCloseTag (reader, "followed-by");
        } else {
          throw new ParsingErrorException ("Node: unknown tag");
        }
      }
      FileUtilities.acceptCloseTag (reader, "node");

      // must at minimum have reference, contents and image
      if (reference == -1 || contents == null || image == null) {
        throw new ParsingErrorException ("Node: not complete");
      }

      return new ReadNode (reference, contents, image, children, namedBy, followedBy);
    }
  }

  /** 
   * Read all the nodes up to the close tag.  Construct a Chrest Node with correct object references.
   */
  private static Node readNodeFromFile (Chrest model, List<ReadNode> allReadNodes, Map<Integer, Node> nodes, 
      BufferedReader reader, String closeTag) throws ParsingErrorException {

    // -- store a list of read nodes, just for this set
    List<ReadNode> readNodes = new ArrayList<ReadNode> ();

    // 1. Read in all the nodes
    while (!FileUtilities.checkCloseTag (reader, closeTag)) {
      ReadNode newNode = ReadNode.readNodeFromFile (reader);
      readNodes.add (newNode);
      allReadNodes.add (newNode);
    }
    // 2. Convert into Chrest Nodes and store in map indexed by reference number
    for (ReadNode node : readNodes) {
      nodes.put (node.getReference (), new Node(model, node.getReference (), node.getContents (), node.getImage ()));
    }
    
    // 3. Add in link references
    for (ReadNode node : readNodes) {
      Node sourceNode = nodes.get (node.getReference ());

      // add child links in reverse order, so that original order is preserved
      for (int i = node.getChildren().size () - 1; i >= 0; --i) {
        ReadLink link = node.getChildren().get (i);
        sourceNode.addTestLink (link.getTest (), nodes.get (link.getChild ()));
      }
    }
    
    // 4. Return the root node, which should have reference 0
    return nodes.get (0);
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
    private List<Integer> _fixationsX, _fixationsY, _fixationsType;

    protected Perceiver () {
      _fixationX = 0;
      _fixationY = 0;
      _fieldOfView = 2;
      _lastHeuristic = 0;
      _fixationsX = new ArrayList<Integer> ();
      _fixationsY = new ArrayList<Integer> ();
      _fixationsType = new ArrayList<Integer> ();
      _fixations = new ArrayList<Fixation> ();
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
      if (_visualStm.getCount () >= 1) {
        List<Link> hypothesisChildren = _visualStm.getItem(0).getChildren ();
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
                      _visualStm.replaceHypothesis (link.getChildNode ());
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
      if (!randomItemHeuristic()) randomPlaceHeuristic ();
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
//      System.out.println(_domainSpecifics.normalise (_currentScene.getItems (_fixationX, _fixationY, 2)));
      recogniseAndLearn (_domainSpecifics.normalise (_currentScene.getItems (_fixationX, _fixationY, 2)));
      // NB: template construction is only assumed to occur after training, so 
      // template completion code is not included here
    }

    /**
     * Find the next fixation point using one of the available 
     * heuristics, and simply move the eye to that point.
     */
    public void moveEye () {
      if (ltmHeuristic ()) return;
      moveEyeUsingHeuristics ();
      recognise (_domainSpecifics.normalise (_currentScene.getItems (_fixationX, _fixationY, 2)));
      // Attempt to fill out the slots on the top-node of visual STM with the currently 
      // fixated items
      if (_visualStm.getCount () >= 1) {
        _visualStm.getItem(0).fillSlots (_currentScene.getItems (_fixationX, _fixationY, 2));
      }
    }

    public String getHeuristicDescription () {
      if (_lastHeuristic == 0)
        return "No heuristic";
      else if (_lastHeuristic == 1)
        return "LTM heuristic";
      else if (_lastHeuristic == 2)
        return "Random item heuristic";
      else // if (_lastHeuristic == 3)
        return "Random place heuristic";
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
  }
}

