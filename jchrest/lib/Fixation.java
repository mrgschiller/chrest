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

  public int getX () {
    return _x;
  }

  public int getY () {
    return _y;
  }

}

