// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.lib;

/**
 * The PairedPattern holds two associated patterns.  The typical use for 
 * this structure is within a categorisation experiment, where one pattern 
 * will be the visual pattern to name and the second will be the verbal 
 * pattern to name it.
 *
 * @author Peter C. R. Lane
 */
public class PairedPattern {
  private ListPattern _first, _second;

  public PairedPattern (ListPattern first, ListPattern second) {
    _first = first;
    _second = second;
  }

  public ListPattern getFirst () {
    return _first;
  }

  public ListPattern getSecond () {
    return _second;
  }
}

