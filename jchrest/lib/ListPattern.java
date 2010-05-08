package jchrest.lib;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import jchrest.lib.FileUtilities;

/**
 * The ListPattern is the primary datatype used to represent compound 
 * patterns within Chrest.  A ListPattern holds an ordered list of 
 * instances of other pattern types.  The ListPattern may optionally 
 * indicate that it cannot be extended by setting the _finished flag.
 * Note that once a pattern is 'finished', it cannot be added to.
 *
 * TODO: Think about if ListPatterns can be embedded within ListPatterns
 *       - would have to look inside ListPattern to make the match.
 *
 * @author Peter C. R. Lane
 */
public class ListPattern extends Pattern {
  private List<PrimitivePattern> _list;  // items within the pattern
  private Modality _modality;   // record type of ListPattern
  private boolean _finished;    // marker to indicate if pattern complete

  public ListPattern () {
    this (Modality.VISUAL);
  }

  public ListPattern (Modality modality) {
    _list = new ArrayList<PrimitivePattern> ();
    _modality = modality;
    _finished = false;
  }

  /** 
   * Used in constructing instances by {@link Pattern} class.
   * Add pattern to list, unless the pattern is 'finished'.
   */
  public void add (PrimitivePattern pattern) {
    if (!_finished) {
      _list.add (pattern);
    }
  }

  /**
   * Construct a copy of this pattern, so that it can be modified 
   * without affecting the original.
   */
  public ListPattern clone () {
    ListPattern result = new ListPattern (_modality);
    for (PrimitivePattern pattern : _list) {
      result.add (pattern);
    }
    if (isFinished ()) {
      result.setFinished ();
    }
    return result;
  }

  /**
   * Return the number of patterns held inside the list pattern.
   */
  public int size () {
    return _list.size ();
  }

  /**
   * Check if the list pattern is empty, holding no patterns.
   */
  public boolean isEmpty () {
    return _list.isEmpty ();
  }

  /**
   * Retrieve the indexed item from the list pattern.
   * There is no check on the validity of the index.
   */
  public PrimitivePattern getItem (int index) {
    return _list.get (index);
  }

  /**
   * Accessor method to _finished property.
   */
  public boolean isFinished () {
    return _finished;
  }

  /**
   * Class level method to check if two patterns have the same modality.
   */
  static public boolean isSameModality (ListPattern pattern1, ListPattern pattern2) {
    return pattern1._modality == pattern2._modality;
  }

  /**
   * Accessor to retrieve the modality of the pattern.
   */
  public Modality getModality () {
    return _modality;
  }

  /**
   * Accessor method to check visual modality.
   */
  public boolean isVisual () {
    return _modality == Modality.VISUAL;
  }

  /**
   * Accessor method to check verbal modality.
   */
  public boolean isVerbal () {
    return _modality == Modality.VERBAL;
  }

  /**
   * Accessor method to check action modality.
   */
  public boolean isAction () {
    return _modality == Modality.ACTION;
  }

  /**
   * Convert the modality into a string.
   */
  public String getModalityString () {
    if (isVisual ()) {
      return "Visual";
    } else if (isVerbal ()) {
      return "Verbal";
    } else { // if (isAction ())
      return "Action";
    }
  }

  /**
   * Set the _finished property to true.
   */
  public void setFinished () {
    _finished = true;
  }

  /**
   * Set the _finished property to false.
   */
  public void setNotFinished () {
    _finished = false;
  }

  /** 
   * Two patterns are equal if they contain the same items.
   */
  public boolean equals (ListPattern pattern) { 
    if (_modality != pattern._modality) return false;

    // patterns must be equal size to be equal
    if (size () != pattern.size ()) return false;

    for (int i = 0, n = size (); i < n; ++i) {
      if (!pattern.getItem(i).equalPrimitive(getItem(i))) {
        return false; // false if any item not the same
      }
    }
    // else, they must both have the 'finished' property the same
    return _finished == pattern.isFinished ();
  }

  /** 
   * Two patterns match if they are both ListPatterns and this ListPattern
   * contains a subset of the given pattern. 
   */
  public boolean matches (Pattern givenPattern) {
    if (!(givenPattern instanceof ListPattern)) return false;
    ListPattern pattern = (ListPattern)givenPattern;

    if (_modality != pattern._modality) return false;

    // check relative sizes of patterns
    if (isFinished ()) {
      if (size () != pattern.size ()) return false;
      if (!pattern.isFinished ()) return false;

    } else {
      // this pattern cannot be larger than given pattern to match it.
      if (size () > pattern.size ()) return false;
    }
    // now just check that the items in this pattern match up with the given pattern
    for (int i = 0, n = size (); i < n; ++i) {
      if (!pattern.getItem(i).equalPrimitive(getItem (i))) {
        return false; // false if any item not the same
      }
    }
    return true;

  }

  /**
   * Return a new ListPattern forming the parts of this pattern without 
   * the matching elements of the given pattern. 
   */
  public ListPattern remove (ListPattern pattern) {
    ListPattern result = new ListPattern (_modality);

    int i = 0;
    boolean takingItems = false;
    while (i < size ()) {
      if (takingItems) {
        result.add (getItem (i));
      } else if (i < pattern.size () && pattern.getItem(i).equalPrimitive(getItem (i))) {
        ;
      } else {
        takingItems = true;
        result.add (getItem (i));
      }
      i += 1;
    }
    if (isFinished () && !(result.isEmpty () && pattern.isFinished ())) {
      result.setFinished ();
    }

    return result;
  }

  /**
   * Return a new ListPattern formed from the contents of this list pattern and the 
   * contents of the given pattern appended to it.
   */
  public ListPattern append (ListPattern pattern) {
    ListPattern result = new ListPattern (_modality);

    for (PrimitivePattern item : _list) {
      result.add (item);
    }

    for (int i = 0, n = pattern.size (); i < n; ++i) {
      result.add (pattern.getItem (i));
    }

    if (pattern.isFinished ()) {
      result.setFinished ();
    }

    return result;
  }

  /** Return a new ListPattern formed from the contents of this list pattern and 
   * the given PrimitivePattern appended to it.
   */
  public ListPattern append (PrimitivePattern pattern) {
    ListPattern result = new ListPattern (_modality);

    for (PrimitivePattern item : _list) {
      result.add (item);
    }
    result.add (pattern);

    return result;
  }

  /**
   * Construct a new pattern containing just the first item in this one.
   */
  public ListPattern getFirstItem () {
    ListPattern pattern = new ListPattern (_modality);
    if (size () > 0) {
      pattern.add (getItem (0));
    }
    pattern.setFinished ();

    return pattern;
  }

  /**
   * Render the list pattern as a string.
   */
  public String toString () {
    String result = "< ";
    for (PrimitivePattern pattern : _list) {
      result += pattern.toString () + " ";
    }
    if (_finished) result += "$ ";

    return result + ">";
  }

  private boolean contains (PrimitivePattern given) {
    for (PrimitivePattern item : _list) {
      if (item.equalPrimitive (given)) return true;
    }
    return false;
  }

  /**
   * Compare this list pattern with a given list pattern, returning true if 
   * the two share k or more items.
   */
  public boolean isSimilarTo (ListPattern pattern, int k) {
    int count = 0;

    for (PrimitivePattern item : _list) {
      if (pattern.contains (item)) { // TODO: Think about duplicate items!
        count += 1;
      }
      if (count >= k) return true;
    }

    return false;
  }

  /**
   * Return a new list pattern with the items sorted using the given comparator.
   */
  public ListPattern sort (Comparator<PrimitivePattern> comparator) {
    ListPattern result = new ListPattern (_modality);
    List<PrimitivePattern> items = new ArrayList<PrimitivePattern> ();
    for (PrimitivePattern pattern : _list) {
      items.add (pattern);
    }
    Collections.sort (items, comparator);
    for (PrimitivePattern pattern : items) {
      result.add (pattern);
    }
    if (isFinished ()) {
      result.setFinished ();
    }
    return result;
  }

  /**
   * Write a description of the list pattern to the given Writer object.
   */
  public void writePattern (Writer writer) throws IOException {
    FileUtilities.writeOpenTag (writer, "list-pattern");
    FileUtilities.writeOpenTag (writer, "items");
    for (PrimitivePattern pattern : _list) {
      pattern.writePattern (writer);
    }
    FileUtilities.writeCloseTag (writer, "items");

    FileUtilities.writeTaggedString (writer, "modality", _modality.toString ());
    FileUtilities.writeTaggedBoolean (writer, "finished", _finished);
    FileUtilities.writeCloseTag (writer, "list-pattern");
  }

  /**
   * Retrieve a description of list pattern from given Writer object, and create it.
   */
  public static ListPattern readPattern (BufferedReader reader) throws ParsingErrorException {
    boolean finished = false;
    Modality modality = Modality.VISUAL;
    List<PrimitivePattern> items = new ArrayList<PrimitivePattern> ();

    FileUtilities.acceptOpenTag (reader, "list-pattern");

    while (!FileUtilities.checkCloseTag (reader, "list-pattern")) {
      if (FileUtilities.checkOpenTag (reader, "items")) {
        FileUtilities.acceptOpenTag (reader, "items");
        while (!FileUtilities.checkCloseTag (reader, "items")) {
          if (FileUtilities.checkOpenTag (reader, "string-pattern")) {
            items.add (StringPattern.readPattern (reader));
          } else if (FileUtilities.checkOpenTag (reader, "number-pattern")) {
            items.add (NumberPattern.readPattern (reader));
          } else if (FileUtilities.checkOpenTag (reader, "item-on-square-pattern")) {
            items.add (ItemSquarePattern.readPattern (reader));
          } else {
            throw new ParsingErrorException ("list-pattern");
          }
        }
        FileUtilities.acceptCloseTag (reader, "items");
      } else if (FileUtilities.checkOpenTag (reader, "modality")) {
        String mode = FileUtilities.readStringInTag (reader, "modality");
        if (mode.equals("VISUAL")) {
          modality = Modality.VISUAL;
        } else if (mode.equals("VERBAL")) {
          modality = Modality.VERBAL;
        } else { // if (mode.equals("ACTION")) 
          modality = Modality.ACTION;
        }
      } else if (FileUtilities.checkOpenTag (reader, "finished")) {
        finished = FileUtilities.readBooleanInTag (reader, "finished");
      } else { // unknown tag
        throw new ParsingErrorException ();
      }
    }

    FileUtilities.acceptCloseTag (reader, "list-pattern");
    ListPattern pattern = new ListPattern (modality);
    for (PrimitivePattern pat : items) {
      pattern.add (pat);
    }
    if (finished) pattern.setFinished ();

    return pattern;
  }
}

enum Modality { VISUAL, VERBAL, ACTION }
