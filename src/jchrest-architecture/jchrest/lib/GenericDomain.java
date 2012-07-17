// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.lib;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jchrest.architecture.Chrest;

/**
  * The GenericDomain is used when no domain-specific methods have been created.
  */
public class GenericDomain implements DomainSpecifics {
  /**
   * No change to pattern, as no definition of normalise.
   */
  public ListPattern normalise (ListPattern pattern) {
    return pattern;
  }

  /** 
   * Return a random square on scene.
   */
  public Set<Square> proposeSalientSquareFixations (Scene scene, Chrest model) {
    Set<Square> result = new HashSet<Square> ();
    result.add (new Square (
          (new java.util.Random()).nextInt (scene.getHeight ()), 
          (new java.util.Random()).nextInt (scene.getWidth ()) 
          ));
    return result;
  }

  /**
   * No possible movement fixations, so return empty list of proposals.
   */
  public List<Square> proposeMovementFixations (Scene scene, Square square) {
    return new ArrayList<Square> ();
  }
}
