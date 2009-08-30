package jchrest.architecture;

import java.util.ArrayList;
import java.util.List;

import jchrest.lib.ListPattern;
import jchrest.lib.Pattern;

public class Node {
  private ListPattern _contents;
  private ListPattern _image;
  private List<Link> _children;

  public Node () {
    _contents = Pattern.makeList (new String[]{});
    _image = Pattern.makeList (new String[]{});
    _children = new ArrayList<Link> ();
  }

  public Node (ListPattern contents, ListPattern image) {
    _contents = contents;
    _image = image;
    _children = new ArrayList<Link> ();
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
   * Accessor to children of node.
   */
  public List<Link> getChildren () {
    return _children;
  }

  /**
   * LearnPrimitive is used to construct a test link and node containing 
   * precisely the given pattern.  It is assumed the given pattern contains 
   * a single primitive item, and is finished.
   */
  public Node learnPrimitive (Chrest model, ListPattern pattern) {
    assert (pattern.isFinished () && pattern.size () == 1);
    Node child = new Node (pattern, pattern);
    _children.add (new Link (pattern, child));

    return child;
  }

  /**
   * Discrimination learning extends the LTM network by adding new 
   * nodes.
   */
  Node discriminate (Chrest model, ListPattern pattern) {
    ListPattern newInformation = pattern.remove (_image);

    // nothing to learn from, so stop
    if (newInformation.isEmpty ()) return this;
    
    Node retrievedChunk = model.recognise (pattern);
    if (retrievedChunk == model.getLtm ()) {
      // if root node is retrieved, then the primitive must be learnt
      return model.getLtm().learnPrimitive (model, pattern.getFirstItem ());
    } else if (retrievedChunk.getImage().isEmpty ()) {
      // if the retrieved chunk has an empty image, then familiarisation must occur
      // to extend that image.
      return retrievedChunk.familiarise (model, pattern);
    } else if (retrievedChunk.getImage().matches (newInformation)) {
      // else, create a new test link using the provided chunk as a test
      Node newChild = new Node (_contents.append (retrievedChunk.getImage ()), new ListPattern ());
      _children.add (new Link (retrievedChunk.getImage (), newChild));
      return newChild;
    } else { // look for or learn a new primitive chunk
      ListPattern primitive = newInformation.getFirstItem ();
      retrievedChunk = model.recognise (primitive);
      if (retrievedChunk == model.getLtm ()) {
        return model.getLtm().learnPrimitive (model, primitive);
      } else {
        Node newChild = new Node (_contents.append (primitive), new ListPattern ());
        _children.add (new Link (primitive, newChild));
        return newChild;
      }
    }
  }

  /**
   * Familiarisation learning extends the image in a node by adding new 
   * information from the given pattern.
   */
  Node familiarise (Chrest model, ListPattern pattern) {
    ListPattern newInformation = pattern.remove (_image);

    if (newInformation.isEmpty ()) {
      // if there is no more information, check if new pattern is 'complete'
      // and make this image complete, if so.
      if (newInformation.isFinished ()) {
        _image.setFinished ();
      }
    } else {
      Node retrievedChunk = model.recognise (newInformation);
      if (retrievedChunk == model.getLtm ()) {
        return model.getLtm().learnPrimitive (model, newInformation.getFirstItem ());
      } else if (retrievedChunk.getImage().isEmpty ()) {
        // could not retrieve useful chunk, so look for, or learn, a primitive
        ListPattern primitive = newInformation.getFirstItem ();
        retrievedChunk = model.recognise (primitive);
        if (retrievedChunk == model.getLtm ()) {
          return model.getLtm().learnPrimitive (model, primitive);
        } else {
          _image = _image.append (primitive);
        }
      } else if (retrievedChunk.getImage().matches (newInformation)) {
        _image = _image.append (retrievedChunk.getImage ());
      } else { // could not retrieve a chunk, so look for, or learn a primitive
        ListPattern primitive = newInformation.getFirstItem ();
        retrievedChunk = model.recognise (primitive);
        if (retrievedChunk == model.getLtm ()) {
          return model.getLtm().learnPrimitive (model, primitive);
        } else {
          _image = _image.append (primitive);
        }
      }
    }

    return this;
  }
}

