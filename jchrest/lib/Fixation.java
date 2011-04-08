package jchrest.lib;

public class Fixation {
  private int _type;
  private int _x;
  private int _y;

  public Fixation (int type, int x, int y) {
    _type = type;
    _x = x;
    _y = y;
  }

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

  public static String getHeuristicDescription (int type) {
    return _descriptions[type];
  }

  public String getHeuristicDescription () {
    return _descriptions[_type];
  }
 
  public int getX () {
    return _x;
  }

  public int getY () {
    return _y;
  }

  /**
   * Note: added 1 to x,y coordinates to match up with the display of visual scenes.
   */
  public String toString () {
    return "(" + (_x+1) + ", " + (_y+1) + ") " + getHeuristicDescription ();
  }

}

