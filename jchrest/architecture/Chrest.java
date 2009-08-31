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
  private int _ltmSize;
  // short-term-memory holds information within the model temporarily, usually within one experiment
  private Stm _stm;

  public Chrest () {
    _ltm = new Node ();
    _stm = new Stm ();
  }

  /**
   * Accessor to retrieve long-term memory of model.
   */
  public Node getLtm () {
    return _ltm;
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
   * Clear the STM and LTM of the model.
   */
  public void clear () {
    _ltm = new Node ();
    setChanged ();
    notifyObservers ();
  }
}

