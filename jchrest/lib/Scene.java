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
    return _scene[row][column];
  }
  
  /**
   * Retrieve all items within given row +/- size, column +/- size
   * TODO: Convert this to use a circular field of view.
   */
  public ListPattern getItems (int row, int column, int size) {
    ListPattern items = new ListPattern ();

    for (int i = column - size; i < column + size; ++i) {
      if (i >= 0 && i < _height) {
        for (int j = row - size; j < row + size; ++j) {
          if (j >= 0 && j < _width) {
            if (!_scene[i][j].equals(".")) {
              items.add (new ItemSquarePattern (_scene[i][j], i, j));

            }
          }
        }
      }
    }

    return items;
  }
}

