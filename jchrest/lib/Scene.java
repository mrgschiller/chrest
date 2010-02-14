package jchrest.lib;

// TODO: Clarify order of row/column in methods calls/displays.
public class Scene {
  private String _name;
  private int _height;
  private int _width;
  private String[][] _scene;

  public Scene (String name, int height, int width) {
    _name = name;
    _height = height;
    _width = width;
    _scene = new String[_height][_width];
  }

  public String getName () {
    return _name;
  }

  public int getHeight () {
    return _height;
  }

  public int getWidth () {
    return _width;
  }

  public void addRow (int row, char [] items) {
    for (int i = 0; i < items.length; ++i) {
      _scene[row][i] = items[i] + "";
    }
  }

  public String getItem (int row, int column) {
    if (row >= 0 && row < _height && column >= 0 && column < _width) {
      return _scene[row][column];
    } else {
      return "";
    }
  }

  public void setItem (int row, int column, String item) {
    _scene[row][column] = item;
  }

  public boolean isEmpty (int row, int column) {
    if (row >= 0 && row < _height && column >= 0 && column < _width) {
      return _scene[row][column].equals (".");
    } else {
      return true; // no item off scene (!)
    }
  }
  
  /**
   * Retrieve all items within given row +/- size, column +/- size
   * TODO: Convert this to use a circular field of view.
   */
  public ListPattern getItems (int startRow, int startColumn, int size) {
    ListPattern items = new ListPattern ();

    for (int col = startColumn - size; col <= startColumn + size; ++col) {
      if (col >= 0 && col < _width) {
        for (int row = startRow - size; row <= startRow + size; ++row) {
          if (row >= 0 && row < _height) {
            if (!_scene[row][col].equals(".")) {
              items.add (new ItemSquarePattern (_scene[row][col], col+1, row+1));

            }
          }
        }
      }
    }

    return items;
  }
}

