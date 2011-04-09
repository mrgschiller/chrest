package jchrest.lib;

/**
 * A Fixation is an (x,y) location on a scene, and the type records the reason for the 
 * fixation being made.
 */
public class Fixation {

  /**
   * Constructor for fixation.
   */
  public Fixation (int type, int x, int y) {
    _type = type;
    _x = x;
    _y = y;
  }

  /**
   * Retrieve the type of fixation.
   */
  public int getType () {
    return _type;
  }

  private final static String[] _descriptions = 
    new String[] {
      "No heuristic", 
      "LTM heuristic", 
      "Random item heuristic", 
      "Random place heuristic",
      "Proposed movement heuristic"
    };

  /**
   * Static method to retrieve description of a given heuristic type.
   */
  public static String getHeuristicDescription (int type) {
    return _descriptions[type];
  }

  /**
   * Retrieve a string describing this fixation.
   */
  public String getHeuristicDescription () {
    return _descriptions[_type];
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
    return "(" + (_x+1) + ", " + (_y+1) + ") " + getHeuristicDescription ();
  }

  // private fields
  private final int _type;
  private final int _x;
  private final int _y;
}

