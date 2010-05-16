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
    // make scene empty to start
    for (int row = 0; row < _height; row++) {
      for (int col = 0; col < _width; col++) {
        _scene[row][col] = ".";
      }
    }
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

  /**
   * Count the number of non-empty squares in the scene.
   */
  public int countItems () {
    int items = 0;
    for (int row = 0; row < _height; row++) {
      for (int col = 0; col < _width; col++) {
        if (isEmpty (row, col)) {
          ;
        } else {
          items += 1;
        }
      }
    }
    return items;
  }

  public int countOverlappingPieces (Scene scene) {
    int items = 0;
    for (int row = 0; row < _height; row++) {
      for (int col = 0; col < _width; col++) {
        if (isEmpty (row, col)) {
          ;
        } else if (_scene[row][col].equals (scene.getItem (row, col))) {
          items += 1;
        } else {
          ;
        }
      }
    }
    return items;
  }         

  /**
   * Compute precision of given scene against this one.
   */
  public float computePrecision (Scene scene) {
    return (float)countOverlappingPieces(scene) / (float)scene.countItems();
  }

  /**
   * Compute recall of given scene against this one.
   */
  public float computeRecall (Scene scene) {
    return (float)countOverlappingPieces(scene) / (float)this.countItems();
  }
  
  /**
   * Compute errors of omission of given scene against this one.
   */
  public int computeErrorsOfOmission (Scene scene) {
    int errors = 0;
    for (int row = 0; row < _height; row++) {
      for (int col = 0; col < _width; col++) {
        if (isEmpty(row, col)) {
          ; // do nothing for empty squares
        } else if (_scene[row][col].equals (scene.getItem (row, col))) {
          ; // no error if this and given scene have the same item
        } else { // an item in this scene is not in given scene
          errors += 1;
        }
      }
    }
    return errors;
  }

  /**
   * Compute errors of commission of given scene against this one.
   */
  public int computeErrorsOfCommission (Scene scene) {
    int errors = 0;
    for (int row = 0; row < _height; row++) {
      for (int col = 0; col < _width; col++) {
        if (scene.isEmpty (row, col)) {
          ; // do nothing for empty squares in given scene
        } else if (scene.getItem(row, col).equals (_scene[row][col])) {
          ; // no error if given and this scene have the same item
        } else { // an item in given scene is not in this scene
          errors += 1;
        }
      }
    }

    return errors;
  }
}

