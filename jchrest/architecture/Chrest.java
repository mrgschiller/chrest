package jchrest.architecture;

import java.util.List;
import java.util.Observable;

import jchrest.lib.ListPattern;

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
  private Node _ltm;
  // short-term-memory holds information within the model temporarily, usually within one experiment
  private int _visualStmSize;
  private int _verbalStmSize;
  private Stm _stm;

  public Chrest () {
    _addLinkTime = 10000;
    _discriminationTime = 10000;
    _familiarisationTime = 2000;
    _rho = 1.0f;
    _visualStmSize = 4;
    _verbalStmSize = 2;

    _clock = 0;
    _ltm = new Node ();
    _stm = new Stm ();
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
    return _visualStmSize;
  }

  /**
   * Modify size of visual short-term memory.
   * TODO: Update STM itself.
   */
  public void setVisualStmSize (int size) {
    _visualStmSize = size;
  }

  /**
   * Accessor to retrieve the size of verbal short-term memory.
   */
  public int getVerbalStmSize () {
    return _verbalStmSize;
  }

  /**
   * Modify size of verbal short-term memory.
   * TODO: Update STM itself.
   */
  public void setVerbalStmSize (int size) {
    _verbalStmSize = size;
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
  }

  /**
   * Accessor to retrieve long-term memory of model.
   */
  public Node getLtm () {
    return _ltm;
  }

  /** 
   * Return a count of the number of nodes in long-term memory.
   */
  public int ltmSize () {
    return _ltm.size ();
  }

  /** 
   * Retrieve a node in long-term memory using the given ListPattern.
   * The sorting process works through the children of the currentNode.
   * If the link's test matches the remaining part of the pattern, then 
   * the current node is updated, and searching continues through the 
   * children of the new node.
   */
  public Node recognise (ListPattern pattern) {
    Node currentNode = _ltm;
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

    return currentNode;
  }

  /** 
   * Use given ListPattern to perform a step of learning within the network.
   * First, the pattern is sorted.  Then, if the retrieved node is the 
   * root node or its image mismatches the pattern, discrimination is 
   * used to extend the network.  Otherwise, new information will be added 
   * to the image using the pattern.
   */
  public Node recogniseAndLearn (ListPattern pattern) {
    Node currentNode = recognise (pattern);
    if (currentNode == _ltm || !currentNode.getImage().matches (pattern)) {
      currentNode = currentNode.discriminate (this, pattern);
      setChanged ();
    } else if (!currentNode.getImage().equals (pattern)) {
      currentNode = currentNode.familiarise (this, pattern);
      setChanged ();
    }
    notifyObservers ();
    return currentNode;
  }

  /**
   * Asks Chrest to return the image of the node obtained by sorting given 
   * pattern through the network.
   */
  public ListPattern recallPattern (ListPattern pattern) {
    return recognise(pattern).getImage ();
  }

  /**
   * Presents Chrest with a pair of patterns, which it should learn and 
   * then attempt to learn a link.
   */
  public void learnAndLinkPatterns (ListPattern pattern1, ListPattern pattern2, int time) {
    recogniseAndLearn (pattern1);
    recogniseAndLearn (pattern2);
    // TODO learn links
  }

  /**
   * Learns the two patterns assuming the time of presentation is the current 
   * Chrest clock time.
   */
  public void learnAndLinkPatterns (ListPattern pattern1, ListPattern pattern2) {
    learnAndLinkPatterns (pattern1, pattern2, _clock);
  }

  /** 
   * Clear the STM and LTM of the model.
   */
  public void clear () {
    _clock = 0;
    _ltm = new Node ();
    setChanged ();
    notifyObservers ();
  }
}

