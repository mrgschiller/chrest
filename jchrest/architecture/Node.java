package jchrest.architecture;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Observable;

import jchrest.lib.FileUtilities;
import jchrest.lib.ItemSquarePattern;
import jchrest.lib.ListPattern;
import jchrest.lib.ParsingErrorException;
import jchrest.lib.Pattern;
import jchrest.lib.PrimitivePattern;

/**
 * Represents a node within the model's long-term memory discrimination network.
 * Methods support learning and also display.
 *
 * @author Peter C. R. Lane
 */
public class Node extends Observable {

  /**
   * Constructor to construct a new root node for the model.  
   */
  public Node (Chrest model, int reference, ListPattern type) {
    this (model, reference, type, type);
  }
 
  /**
   * When constructing non-root nodes in the network, the new contents and image 
   * must be defined.  Assume that the image always starts empty.
   */
  public Node (Chrest model, ListPattern contents, ListPattern image) {
    this (model, model.getNextNodeNumber (), contents, image);
  }

  /**
   * Constructor to build a new Chrest node with given reference, contents and image.
   * Package access only, as should only be used by Chrest.java.
   */
  Node (Chrest model, int reference, ListPattern contents, ListPattern image) {
    _model = model;
    _reference = reference;
    _contents = contents.clone ();
    _contents.setNotFinished (); // do not allow contents to be finished
    _image = image;
    _children = new ArrayList<Link> ();
    _similarNodes = new ArrayList<Node> ();
    _followedBy = null;
    _namedBy = null;
  }

  /**
   * When the model is reset, all observers of individual nodes must be closed.
   * This method notifies observers to close themselves, and then 
   * requests child nodes to do the same.
   */
  void clear () {
    setChanged ();
    notifyObservers ("close");
    for (Link child : _children) {
      child.getChildNode().clear ();
    }
  }

  /**
   * Accessor to reference number of node.
   */
  public int getReference () {
    return _reference;
  }

  /**
   * Accessor to contents of node.
   */
  public ListPattern getContents () {
    return _contents;
  }

  /**
   * Accessor to image of node.
   */
  public ListPattern getImage () {
    return _image;
  }

  /**
   * Change the node's image.  Also notifies any observers.
   */
  public void setImage (ListPattern image) {
    _image = image;
    setChanged ();
    notifyObservers ();
  }

  /**
   * Accessor to children of node.
   */
  public List<Link> getChildren () {
    return _children;
  }

  /**
   * Add a new test link with given test pattern and child node.
   */
  void addTestLink (ListPattern test, Node child) {
    _children.add (0, new Link (test, child));
    setChanged ();
    notifyObservers ();
  }

  /**
   * Add a node to the list of similar nodes.  Do not add duplicates.
   */
  void addSimilarNode (Node node) {
    if (!_similarNodes.contains (node)) {
      _similarNodes.add (node);
      setChanged ();
      notifyObservers ();
    }
  }

  /**
   * Accessor to list of similar nodes.
   */
  public List<Node> getSimilarNodes () {
    return _similarNodes;
  }

  /**
   * Accessor to node that follows this node.
   */
  public Node getFollowedBy () {
    return _followedBy;
  }

  /**
   * Modify node that follows this node.
   */
  public void setFollowedBy (Node node) {
    _followedBy = node;
    setChanged ();
    notifyObservers ();
  }

  /**
   * Accessor to node that names this node.
   */
  public Node getNamedBy () {
    return _namedBy;
  }

  /**
   * Modify node that names this node.
   */
  public void setNamedBy (Node node) {
    _namedBy = node;
    setChanged ();
    notifyObservers ();
  }

  /** 
   * Compute the size of the network below the current node.
   */
  public int size () {
    int count = 1; // for self
    for (Link link : _children) {
      count += link.getChildNode().size ();
    }

    return count;
  }

  /**
   * Compute the amount of information in current node.  
   * Information is based on the size of the image + the number of slots.
   */
  public int information () {
    int information = _image.size ();
    if (_itemSlots != null) {
      information += _itemSlots.size ();
    }
    if (_positionSlots != null) {
      information += _positionSlots.size ();
    }

    return information;
  }

  /**
   * Add to a map of content sizes to node counts for this node and its children.
   */
  protected void getContentCounts (Map<Integer, Integer> size) {
    int csize = _contents.size ();
    if (size.containsKey (csize)) {
      size.put (csize, size.get(csize) + 1);
    } else {
      size.put (csize, 1);
    }

    for (Link child : _children) {
      child.getChildNode().getContentCounts (size);
    }
  }

  /**
   * Add to a map of image sizes to node counts for this node and its children.
   */
  protected void getImageCounts (Map<Integer, Integer> size) {
    int csize = _image.size ();
    if (size.containsKey (csize)) {
      size.put (csize, size.get(csize) + 1);
    } else {
      size.put (csize, 1);
    }

    for (Link child : _children) {
      child.getChildNode().getImageCounts (size);
    }
  }

  /**
   * Add to a map from number of similarity nodes to frequency, for this node and its children.
   */
  protected void getSimilarityCounts (Map<Integer, Integer> size) {
    int csize = _similarNodes.size ();
    if (csize > 0) { // do not count nodes with no similarity links
      if (size.containsKey (csize)) {
        size.put (csize, size.get(csize) + 1);
      } else {
        size.put (csize, 1);
      }
    }

    for (Link child : _children) {
      child.getChildNode().getSimilarityCounts (size);
    }
  }
  
  // private fields
  private final Chrest _model;
  private final int _reference;
  private final ListPattern _contents;
  private ListPattern _image;
  private List<Link> _children;
  private List<Node> _similarNodes;
  private Node _followedBy;
  private Node _namedBy;

  /**
   * Compute the total size of images below the current node.
   */
  private int totalImageSize () {
    int size = _image.size ();
    for (Link link : _children) {
      size += link.getChildNode().totalImageSize ();
    }

    return size;
  }

  /**
   * If this node is a child node, then add its depth to depths.  
   * Otherwise, continue searching through children for the depth.
   */
  private void findDepth (int currentDepth, List<Integer> depths) {
    if (_children.isEmpty ()) {
      depths.add (currentDepth);
    } else {
      for (Link link : _children) {
        link.getChildNode().findDepth (currentDepth + 1, depths);
      }
    }
  }

  /**
   * Compute the average depth of nodes below this point.
   */
  public double averageDepth () {
    List<Integer> depths = new ArrayList<Integer> ();
    // -- find every depth
    for (Link link : _children) {
      link.getChildNode().findDepth(1, depths);
    }

    // -- compute the average of the depths
    int sum = 0;
    for (Integer depth : depths) {
      sum += depth;
    }
    if (depths.isEmpty ()) {
      return 0.0;
    } else {
      return (double)sum / (double)depths.size ();
    }
  }

  /**
   * Compute the average size of the images in nodes below this point.
   */
  public double averageImageSize () {
    return (double)totalImageSize() / size();
  }

  /**
   * Count templates in part of network rooted at this node.
   */
  public int countTemplates () {
    int count = 0;
    if (isTemplate ()) count += 1;

    for (Link link : _children) {
      count += link.getChildNode().countTemplates ();
    }

    return count;
  }

  private List<ItemSquarePattern> _itemSlots;
  private List<ItemSquarePattern> _positionSlots;
  private List<ItemSquarePattern> _filledItemSlots;
  private List<ItemSquarePattern> _filledPositionSlots;

  public List<ItemSquarePattern> getFilledItemSlots () {
    return _filledItemSlots;
  }

  public List<ItemSquarePattern> getFilledPositionSlots () {
    return _filledPositionSlots;
  }

  /**
   * Returns true if this node is a template.  To be a template, the node 
   * must be at least one slot of any kind.
   */
  public boolean isTemplate () {
    if (_itemSlots == null || _positionSlots == null) {
      return false;
    }

    // is a template if there is at least one slot
    if (_itemSlots.size () > 0) return true;
    if (_positionSlots.size () > 0) return true;

    return false;
  }

  /**
   * Clear out the template slots.
   */
  public void clearTemplate () {
    if (_itemSlots != null) _itemSlots.clear ();
    if (_positionSlots != null) _positionSlots.clear ();
  }

  /**
   * Attempt to fill some of the slots using the items in the given pattern.
   */
  public void fillSlots (ListPattern pattern) {
    if (_itemSlots == null) _itemSlots = new ArrayList<ItemSquarePattern> ();
    if (_positionSlots == null) _positionSlots = new ArrayList<ItemSquarePattern> ();
    if (_filledItemSlots == null) _filledItemSlots = new ArrayList<ItemSquarePattern> ();
    if (_filledPositionSlots == null) _filledPositionSlots = new ArrayList<ItemSquarePattern> ();

    for (int index = 0; index < pattern.size (); index++) {
      if (pattern.getItem(index) instanceof ItemSquarePattern) {
        ItemSquarePattern item = (ItemSquarePattern)(pattern.getItem (index));
        // only try to fill a slot if item is not already in image
        if (!_image.contains (item)) { 
          // 1. check the item slots
          for (ItemSquarePattern slot : _itemSlots) {
            if (slot.getItem().equals(item.getItem ())) {
              _filledItemSlots.add (item);
            }
          }

          // 2. check the position slots
          for (ItemSquarePattern slot : _positionSlots) {
            if (slot.getRow () == item.getRow () &&
                slot.getColumn () == item.getColumn ()) {
              _filledPositionSlots.add (item);
                }
          }
        }
      }
    }
  }

  public void clearFilledSlots () {
    if (_filledItemSlots == null) _filledItemSlots = new ArrayList<ItemSquarePattern> ();
    if (_filledPositionSlots == null) _filledPositionSlots = new ArrayList<ItemSquarePattern> ();

    _filledItemSlots.clear ();
    _filledPositionSlots.clear ();
  }

  /**
   * Retrieve all primitive items stored in slots of template as a ListPattern.
   * The retrieved pattern may contain duplicate primitive items, but will be 
   * untangled in Chrest#scanScene.
   */
  ListPattern getFilledSlots () {
    ListPattern filledSlots = new ListPattern ();
    for (ItemSquarePattern filledSlot : _filledItemSlots) {
      filledSlots.add (filledSlot);
    }
    for (ItemSquarePattern filledSlot : _filledPositionSlots) {
      filledSlots.add (filledSlot);
    }
    return filledSlots;
  }

  /**
   * Converts this node into a template, if appropriate, and repeats for 
   * all child nodes.
   * Note: usually, this process is done as a whole at the end of training, but 
   * can also be done on a node-by-node basis, during training.
   */
  public void constructTemplates () {
    _itemSlots = new ArrayList<ItemSquarePattern> ();
    _positionSlots = new ArrayList<ItemSquarePattern> ();

    if (canFormTemplate ()) {
      // gather images of current node, test links and similar nodes together, 
      // removing the contents from them
      List<ListPattern> patterns = new ArrayList<ListPattern> ();
      patterns.add (_image.remove (_contents));
      for (Link link : _children) {
        patterns.add (link.getChildNode().getImage().remove (_contents));
      }
      for (Node node : _similarNodes) {
        patterns.add (node.getImage().remove (_contents));
      }
      // create a hashmap of counts of occurrences of items and of squares
      Map<String,Integer> countItems = new HashMap<String,Integer> ();
      Map<Integer,Integer> countPositions = new HashMap<Integer,Integer> ();
      for (ListPattern pattern : patterns) {
        for (PrimitivePattern pattern_item : pattern) {
          if (pattern_item instanceof ItemSquarePattern) {
            ItemSquarePattern item = (ItemSquarePattern)pattern_item;
            if (countItems.containsKey (item.getItem ())) {
              countItems.put (item.getItem (), countItems.get(item.getItem ()) + 1);
            } else {
              countItems.put (item.getItem (), 1);
            }
            // TODO: Check construction of 'posn_key', try 1000 = scene.getWidth ?
            Integer posn_key = item.getRow () + 1000 * item.getColumn ();
            if (countPositions.containsKey (posn_key)) {
              countPositions.put (posn_key, countPositions.get(posn_key) + 1);
            } else {
              countPositions.put (posn_key, 1);
            }
          }
        }
      }

      // make slots
      // 1. from items which repeat more than minimumNumberOccurrences
      for (String itemKey : countItems.keySet ()) {
        if (countItems.get(itemKey) >= _model.getMinTemplateOccurrences ()) {
          _itemSlots.add (new ItemSquarePattern (itemKey, -1, -1));
        }
      }
      // 2. from locations which repeat more than minimumNumberOccurrences
      for (Integer posnKey : countPositions.keySet ()) {
        if (countPositions.get(posnKey) >= _model.getMinTemplateOccurrences ()) {
          _positionSlots.add (new ItemSquarePattern ("slot", posnKey / 1000, posnKey - (1000 * (posnKey/1000))));
        }
      }
    }

    // continue conversion for children of this node
    for (Link link : _children) {
      link.getChildNode().constructTemplates ();
    }

  }

  /** Return true if template conditions are met:
   * 1. contents size > _model.getMinTemplateLevel ()
   * then one of:
   * 2. gather together current node image and images of all nodes linked by the test links
   *    remove the contents of current node from those images
   *    see if any piece or square repeats more than once
   */
  public boolean canFormTemplate () {
    // return false if node is too shallow in network
    if (_contents.size () <= _model.getMinTemplateLevel ()) return false;
    // gather images of current node and test links together, removing the contents from them
    List<ListPattern> patterns = new ArrayList<ListPattern> ();
    patterns.add (_image.remove (_contents));
    for (Link link : _children) {
      patterns.add (link.getChildNode().getImage().remove (_contents));
    }
    // create a hashmap of counts of occurrences of items and of squares
    Map<String,Integer> countItems = new HashMap<String,Integer> ();
    Map<Integer,Integer> countPositions = new HashMap<Integer,Integer> ();
    for (ListPattern pattern : patterns) {
      for (PrimitivePattern pattern_item : pattern) {
        if (pattern_item instanceof ItemSquarePattern) {
          ItemSquarePattern item = (ItemSquarePattern)pattern_item;
          if (countItems.containsKey (item.getItem ())) {
            countItems.put (item.getItem (), countItems.get(item.getItem ()) + 1);
          } else {
            countItems.put (item.getItem (), 1);
          }
          Integer posn_key = item.getRow () + 1000 * item.getColumn ();
          if (countPositions.containsKey (posn_key)) {
            countPositions.put (posn_key, countPositions.get(posn_key) + 1);
          } else {
            countPositions.put (posn_key, 1);
          }
        }
      }
    }

    // make slots
    // 1. from items which repeat more than minimumNumberOccurrences
    for (String itemKey : countItems.keySet ()) {
      if (countItems.get(itemKey) >= _model.getMinTemplateOccurrences ()) {
        return true;
      }
    }
    // 2. from locations which repeat more than minimumNumberOccurrences
    for (Integer posnKey : countPositions.keySet ()) {
      if (countPositions.get(posnKey) >= _model.getMinTemplateOccurrences ()) {
        return true;
      }
    }
    return false;
  }

  /**
   * LearnPrimitive is used to construct a test link and node containing 
   * precisely the given pattern.  It is assumed the given pattern contains 
   * a single primitive item, and is finished.
   */
  public Node learnPrimitive (ListPattern pattern) {
    assert (pattern.isFinished () && pattern.size () == 1);
    ListPattern contents = pattern.clone ();
    contents.setNotFinished ();
    Node child = new Node (_model, contents, pattern);
    addTestLink (contents, child);
    _model.advanceClock (_model.getDiscriminationTime ());

    return child;
  }

  /**
   * addTest is used to construct a test link using the given pattern, 
   * with a new empty child node.  It is assumed the given pattern is 
   * non-empty and constitutes a valid, new test for the current Node.
   */
  private Node addTest (ListPattern pattern) {
    Node child = new Node (_model, _contents.append (pattern), new ListPattern (_contents.getModality ()));
    addTestLink (pattern, child);
    _model.advanceClock (_model.getDiscriminationTime ());
    return child;
  }

  /**
   * extendImage is used to add new information to the node's image.
   * It is assumed the given pattern is non-empty and is a valid extension.
   */
  private Node extendImage (ListPattern newInformation) {
    setImage (_image.append (newInformation));
    _model.advanceClock (_model.getFamiliarisationTime ());

    return this;
  }

  /**
   * Discrimination learning extends the LTM network by adding new 
   * nodes.
   */
  Node discriminate (ListPattern pattern) {
    ListPattern newInformation = pattern.remove (_contents);

    // cases 1 & 2 if newInformation is empty
    if (newInformation.isEmpty ()) {
      if (newInformation.isFinished ()) { // 1. add test for < $ >
        return addTest (newInformation);
      } else { // 2. no information to make a new test with
        return this;
      }
    }

    Node retrievedChunk = _model.recognise (newInformation);
    if (retrievedChunk == _model.getLtmByModality (pattern)) {
      // 3. if root node is retrieved, then the primitive must be learnt
      return _model.getLtmByModality(newInformation).learnPrimitive (newInformation.getFirstItem ());
    } else if (retrievedChunk.getImage().isEmpty ()) {
      // 4. if the retrieved chunk has an empty image, then familiarisation must occur
      // to extend that image.
      return retrievedChunk.familiarise (newInformation);
    } else if (retrievedChunk.getImage().matches (newInformation)) {
      // 5. retrieved chunk can be used as a test
      ListPattern testPattern = retrievedChunk.getImage().clone ();
      testPattern.setNotFinished (); // ensure test link is not finished
      return addTest (testPattern);
    } else { 
      // 6. mismatch, so use only the first item for test
      // NB: first-item must be in network as retrievedChunk was not the root node
      ListPattern firstItem = newInformation.getFirstItem ();
      firstItem.setNotFinished ();
      return addTest (firstItem);
    }
  }

  /**
   * Familiarisation learning extends the image in a node by adding new 
   * information from the given pattern.
   */
  Node familiarise (ListPattern pattern) {
    ListPattern newInformation = pattern.remove (_image);

    // cases 1 & 2 if newInformation is empty
    if (newInformation.isEmpty ()) {
      if (newInformation.isFinished ()) { // 1. add end marker
        return extendImage (newInformation);
      } else {
        // 2. nothing to do
        return this;
      }
    }

    Node retrievedChunk = _model.recognise (newInformation);
    if (retrievedChunk == _model.getLtmByModality (pattern)) {
      // 3. if root node is retrieved, first item of newInformation is an unknown primitive
      return _model.getLtmByModality(pattern).learnPrimitive (newInformation.getFirstItem ());
    } else if (retrievedChunk.getImage().isEmpty ()) {
      // 4. the retrieved chunk is empty, so use first item to extend image
      // note: first item is known primitive, because new-information sorted to this node
      ListPattern firstItem = newInformation.getFirstItem ();
      firstItem.setNotFinished ();
      return extendImage (firstItem);
    } else if (retrievedChunk.getImage().matches (newInformation)) {
      // 5. retrieved chunk an be used to extend the image 
      //   -- make sure extension is not complete
      ListPattern toadd = retrievedChunk.getImage().clone ();
      toadd.setNotFinished ();
      return extendImage (toadd);
    } else { 
      // 6. mismatch, so only use first item to extend image
      // note: mismatch cannot be first item, because new-information sorted to this node
      ListPattern firstItem = retrievedChunk.getImage().getFirstItem ();
      firstItem.setNotFinished ();
      return extendImage (firstItem);
    }
  }

  /**
   * Write node information in VNA format.
   */
  public void writeNodeAsVna (Writer writer) throws IOException {
    writer.write ("" + _reference + " \"" + _contents.toString() + "\"\n");
    for (Link link : _children) {
      link.getChildNode().writeNodeAsVna (writer);
    }
  }

  public void writeLinksAsVna (Writer writer) throws IOException {
    // write my links
    for (Link link : _children) {
      writer.write ("" + _reference + " " + link.getChildNode().getReference () + "\n");
    }
    // repeat for children
    for (Link link : _children) {
      link.getChildNode().writeLinksAsVna (writer);
    }
  }

  public void writeSimilarityLinksAsVna (Writer writer) throws IOException {
    // write my links
    for (Node node : _similarNodes) {
      writer.write ("" + _reference + " " + node.getReference () + "\n");
    }
    // repeat for children
    for (Link link : _children) {
      link.getChildNode().writeSimilarityLinksAsVna (writer);
    }
  }
}

