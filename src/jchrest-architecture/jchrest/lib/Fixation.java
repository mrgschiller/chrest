// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.lib;

/**
 * A Fixation is an (x,y) location on a scene, and the type records the reason for the 
 * fixation being made.
 */
public class Fixation {

  /**
   * Constructor for fixation.
   */
  public Fixation (FixationType type, int x, int y) {
    _type = type;
    _x = x;
    _y = y;
  }

  /**
   * Retrieve the type of fixation.
   */
  public FixationType getType () {
    return _type;
  }

  /**
   * Retrieve a string describing this fixation.
   */
  public String getHeuristicDescription () {
    return _type.toString ();
  }
 
  /**
   * Retrieve the x coordinate of this fixation.
   */
  public int getX () {
    return _x;
  }

  /**
   * Retrieve the y coordinate of this fixation.
   */
  public int getY () {
    return _y;
  }

  /**
   * Note: added 1 to x,y coordinates to match up with the display of visual scenes.
   */
  public String toString () {
    return "(" + (_x+1) + ", " + (_y+1) + ") " + _type;
  }

  // private fields
  private final FixationType _type;
  private final int _x;
  private final int _y;
}

