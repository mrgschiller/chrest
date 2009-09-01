package jchrest.lib;

import java.util.ArrayList;
import java.util.List;

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
  private List<Pattern> _list;  // items within the pattern
  private boolean _finished;    // marker to indicate if pattern complete

  public ListPattern () {
    _list = new ArrayList<Pattern> ();
    _finished = false;
  }

  /** 
   * Used in constructing instances by {@link Pattern} class.
   * Add pattern to list, unless the pattern is 'finished'.
   */
  public void add (Pattern pattern) {
    if (!_finished) {
      _list.add (pattern);
    }
  }

  /**
   * Construct a copy of this pattern, so that it can be modified 
   * without affecting the original.
   */
  public ListPattern clone () {
    ListPattern result = new ListPattern ();
    for (Pattern pattern : _list) {
      result.add (pattern);
    }
    if (isFinished ()) {
      result.setFinished ();
    }
    return result;
  }

  public int size () {
    return _list.size ();
  }

  public boolean isEmpty () {
    return _list.isEmpty ();
  }

  public Pattern getItem (int index) {
    return _list.get (index);
  }

  /**
   * Accessor method to _finished property.
   */
  public boolean isFinished () {
    return _finished;
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
  public boolean equals (Pattern givenPattern) {
    if (!(givenPattern instanceof ListPattern)) return false;
    ListPattern pattern = (ListPattern)givenPattern;

    // patterns must be equal size to be equal
    if (size () != pattern.size ()) return false;

    for (int i = 0, n = size (); i < n; ++i) {
      if (!pattern.getItem(i).equals(getItem(i))) {
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
      if (!pattern.getItem(i).equals(getItem (i))) {
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
    ListPattern result = new ListPattern ();

    int i = 0;
    boolean takingItems = false;
    while (i < size ()) {
      if (takingItems) {
        result.add (getItem (i));
      } else if (i < pattern.size () && pattern.getItem(i).equals(getItem (i))) {
        ;
      } else {
        takingItems = true;
        result.add (getItem (i));
      }
      i += 1;
    }
    if (isFinished () && !pattern.isFinished ()) {
      result.setFinished ();
    }

    return result;
  }

  /**
   * Return a new ListPattern formed from the contents of this list pattern and the 
   * contents of the given pattern appended to it.
   */
  public ListPattern append (ListPattern pattern) {
    ListPattern result = new ListPattern ();

    for (Pattern item : _list) {
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
    ListPattern result = new ListPattern ();

    for (Pattern item : _list) {
      result.add (item);
    }
    result.add (pattern);

    return result;
  }

  /**
   * Construct a new pattern containing just the first item in this one.
   */
  public ListPattern getFirstItem () {
    ListPattern pattern = new ListPattern ();
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
    for (Pattern pattern : _list) {
      result += pattern.toString () + " ";
    }
    if (_finished) result += "$ ";

    return result + ">";
  }
}

