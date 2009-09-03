package jchrest.lib;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * The Scenes class holds a list of Scene objects.  Each Scene contains 
 * an array of 'situations'.
 *
 * @author Peter C. R. Lane
 */
public class Scenes {

  /**
   * Read in a list of scenes from the given input stream.  Format is:
   * line 1: height width              e.g. 8 8
   * line 2: items single character    e.g. KQR ...
   * line 3: blank (can be comment)
   * line 4-4+height: line of length 'width'.  Each char in line 2 or '.' (empty)
   * line 4+height+1: blank (can be comment) and repeat until EOF
   *
   * Throws IOException if any line is short, or the number of lines cannot be read.
   */
  public static Scenes read (BufferedReader input) throws IOException {
    Scenes scenes;
    int height, width;

    String line;
    line = input.readLine ();
    if (line == null) throw new IOException (); 
    String[] dimensions = line.split (" ");
    if (dimensions.length != 2) throw new IOException ();
    try {
      height = Integer.decode(dimensions[0]).intValue ();
      width = Integer.decode(dimensions[1]).intValue ();
    } catch (NumberFormatException nfe) { throw new IOException (); 
    }
    
    scenes = new Scenes (height, width);
    int sceneNumber = 0;
    line = input.readLine (); // read the blank/comment line
    while (line != null) { // finish if no more positions to read
      line = input.readLine (); // read first line of scene
      if (line == null) break;  // finish calmly if last position followed by blank line
      sceneNumber += 1;

      Scene scene = new Scene ("Scene " + sceneNumber, height, width);
      for (int i = 0; i < height; ++i) {
        if (line == null) throw new IOException ();         // finished in the middle of a position
        if (line.length() != width) throw new IOException (); // incorrect width of row
        scene.addRow (i, line.toCharArray ());
        line = input.readLine (); // on last cycle, this tries to read blank/comment line
      }
      scenes.add (scene);
    }

    return scenes;
  }

  private int _height;
  private int _width;
  private List<Scene> _scenes;

  private Scenes (int height, int width) {
    _height = height;
    _width = width;
    _scenes = new ArrayList<Scene> ();
  }

  private void add (Scene scene) {
    _scenes.add (scene);
  }

  public String [] getSceneNames () {
    String [] names = new String[_scenes.size ()];
    for (int i = 0; i < _scenes.size (); ++i) {
      names[i] = _scenes.get(i).getName ();
    }
    return names;
  }

  public Scene get (int i) {
    return _scenes.get (i);
  }
}

