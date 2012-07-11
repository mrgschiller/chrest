// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

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
   * line 2: blank (can be comment)
   * line 3-3+height: line of length 'width'.  Each char in line 2 or '.' (empty)
   * line 3+height+1: blank (can be comment) and repeat until EOF
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

  /**
   * Read in a list of scenes from the given input stream.  Format is:
   * line 1: height width              e.g. 8 8
   * line 2: blank (can be comment)
   * line 3-3+height: line of length 'width'.  Each char in line 2 or '.' (empty)
   * line 3+height+1: piece row column  defines a move (row from 0 at the top, column from 0 at the left)
   * line 3+height+2: blank (can be comment) and repeat until EOF
   *
   * Throws IOException if any line is short, or the number of lines cannot be read.
   */
  public static Scenes readWithMove (BufferedReader input) throws IOException {
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
    } catch (NumberFormatException nfe) { throw new IOException ("Mistake in reading height/width"); 
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
        if (line == null) throw new IOException ("Finished in middle of position");         // finished in the middle of a position
        if (line.length() != width) throw new IOException ("Row is wrong size"); // incorrect width of row
        scene.addRow (i, line.toCharArray ());
        line = input.readLine ();
      }
      // move will be in last line read
      if (line == null) throw new IOException ("Failed to read the move");         // finished before could read the move
      String[] moveDefinition = line.split (" ");
      if (moveDefinition.length != 3) { // make sure definition is the right size
        throw new IOException ("Move definition not the right size");
      }
      Move move;
      try {
        move = new Move (
            moveDefinition[0], 
            Integer.valueOf (moveDefinition[1]), 
            Integer.valueOf (moveDefinition[2]));
      } catch (NumberFormatException nfe) {
        // throw IOException if components of move are not numbers
        throw new IOException ("Components of move definition are not numbers"); 
      }
      // add the scene-move pair
      scenes.add (scene, move);
      // consume blank line at end of definition
      input.readLine (); 
    }

    return scenes;
  }

  private int _height;
  private int _width;
  private List<Scene> _scenes;
  private List<Move> _moves;

  private Scenes (int height, int width) {
    _height = height;
    _width = width;
    _scenes = new ArrayList<Scene> ();
    _moves = new ArrayList<Move> ();
  }

  private void add (Scene scene) {
    _scenes.add (scene);
  }

  private void add (Scene scene, Move move) {
    _scenes.add (scene);
    _moves.add (move);
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

  public boolean haveMoves () {
    return (_moves.size () > 0);
  }

  public Move getMove (int i) {
    if (haveMoves () && i < _moves.size ()) {
      return _moves.get (i);
    } else {
      return new Move ("None", 0, 0);
    }
  }

  public int size () {
    return _scenes.size ();
  }
}

