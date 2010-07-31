package jchrest.lib;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
  * The ChessDomain is used for chess modelling.
  */
public class ChessDomain implements DomainSpecifics {
  public ListPattern normalise (ListPattern pattern) {
    return pattern.sort (new Comparator<PrimitivePattern> () {
      public int compare (PrimitivePattern left, PrimitivePattern right) {
        assert (left instanceof ItemSquarePattern);
        assert (right instanceof ItemSquarePattern);
        ItemSquarePattern leftIos = (ItemSquarePattern)left;
        ItemSquarePattern rightIos = (ItemSquarePattern)right;

        // P p K k B b N n Q q R r - the canonical ordering of chess pieces, from deGroot and Gobet (1996)
        List<String> pieces = new ArrayList<String> ();
        pieces.add ("P"); pieces.add ("p");
        pieces.add ("K"); pieces.add ("k");
        pieces.add ("B"); pieces.add ("b");
        pieces.add ("N"); pieces.add ("n");
        pieces.add ("Q"); pieces.add ("q");
        pieces.add ("R"); pieces.add ("r");

        // check item
        if (pieces.indexOf (leftIos.getItem()) < pieces.indexOf (rightIos.getItem ())) return -1;
        if (pieces.indexOf (leftIos.getItem()) > pieces.indexOf (rightIos.getItem ())) return 1;
        // check column
        if (leftIos.getColumn () < rightIos.getColumn ()) return -1;
        if (leftIos.getColumn () > rightIos.getColumn ()) return 1;
        // check row
        if (leftIos.getRow () < rightIos.getRow ()) return -1;
        if (leftIos.getRow () > rightIos.getRow ()) return 1;
        return 0;
      }
    });
  }

  /**
   * A 'big piece' is anything other than a pawn.  
   * Used to indicate a salient piece for a novice chess player.
   */
  public static boolean isBigPiece (ItemSquarePattern ios) {
    return !(ios.getItem().equals ("P") || ios.getItem().equals ("p"));
  }

  /**
   * An 'offensive piece' is a piece on the other player's side.
   * e.g. a black piece on white's side of the board.
   * Used to indicate a salient piece for an inexperienced chess player.
   */
  public static boolean isOffensivePiece (ItemSquarePattern ios) {
    if (ios.getItem().isEmpty ()) return false;
    char piece = ios.getItem().charAt (0);
    if (Character.isLowerCase (piece) && ios.getRow () <= 4) return true; // black piece on white side
    if (Character.isUpperCase (piece) && ios.getRow () >= 5) return true; // white piece on black side
    return false;
  }

  public static ListPattern getSalientPieces (ListPattern pattern, int isExperienced) {
    return getSalientPieces (pattern, isExperienced == 1);
  }

  /**
   * Retrieve the salient pieces from the given pattern.
   * The 'isExperienced' flag is used to indicate level of skill of model:
   * isExperienced = false means use the 'isBigPiece' measure,
   * isExperienced = true means use the 'isOffensivePiece' measure.
   */
  public static ListPattern getSalientPieces (ListPattern pattern, Boolean isExperienced) {
    ListPattern result = new ListPattern (pattern.getModality ());
    for (int i = 0, n = pattern.size (); i < n; ++i) {
      if (pattern.getItem (i) instanceof ItemSquarePattern) {
        ItemSquarePattern ios = (ItemSquarePattern)(pattern.getItem (i));
        if (isExperienced && isOffensivePiece (ios)) {
          result.add (ios);
        } else if (!isExperienced && isBigPiece (ios)) {
          result.add (ios);
        } else {
          ; // leave out non-salient pieces
        }
      }
    }
    return result;
  }

  private boolean differentColour (Scene board, Square square1, Square square2) {
    char item1 = board.getItem (square1.getRow (), square1.getColumn ()).charAt (0);
    char item2 = board.getItem (square2.getRow (), square2.getColumn ()).charAt (0);

    return 
      (Character.isUpperCase (item1) && Character.isLowerCase (item2)) ||
      (Character.isUpperCase (item2) && Character.isLowerCase (item1));
  }

  // add destination square to given list if the move would be to an empty square, or a capture
  private void addValidMove (Scene board, Square source, Square destination, List<Square> moves) {
    if (board.isEmpty (destination.getRow (), destination.getColumn ()) ||
        differentColour (board, source, destination)) {
      moves.add (destination);
        }
  }

  // compute possible pawn moves
  // -- assume square is position of a black pawn
  private List<Square> findBlackPawnMoves (Scene board, Square square) {
    List<Square> moves = new ArrayList<Square> ();

    // check move forward
    if (board.isEmpty (square.getRow () + 1, square.getColumn ())) {
      moves.add (new Square (square.getRow () + 1, square.getColumn ()));
      // initial move
      if (square.getRow () == 1 && board.isEmpty (square.getRow () + 2, square.getColumn ())) {
        moves.add (new Square (square.getRow () + 2, square.getColumn ()));
      }
    }

    // check captures - e.p. ignored
    if (square.getColumn () > 0) { // not in column a
      Square destination = new Square (square.getRow () + 1, square.getColumn () - 1);
      if (differentColour (board, square, destination)) {
        moves.add (destination);
      }
    }
    if (square.getColumn () < 7) { // not in column h
      Square destination = new Square (square.getRow () + 1, square.getColumn () + 1);
      if (differentColour (board, square, destination)) {
        moves.add (destination);
      }
    }

    return moves;
  }

  // -- assume square is position of a white pawn
  private List<Square> findWhitePawnMoves (Scene board, Square square) {
    List<Square> moves = new ArrayList<Square> ();

    // check move forward
    if (board.isEmpty (square.getRow () - 1, square.getColumn ())) {
      moves.add (new Square (square.getRow () - 1, square.getColumn ()));
      // initial move
      if (square.getRow () == 1 && board.isEmpty (square.getRow () - 2, square.getColumn ())) {
        moves.add (new Square (square.getRow () - 2, square.getColumn ()));
      }
    }

    // check captures - e.p. ignored
    if (square.getColumn () > 0) { // not in column a
      Square destination = new Square (square.getRow () - 1, square.getColumn () - 1);
      if (differentColour (board, square, destination)) {
        moves.add (destination);
      }
    }
    if (square.getColumn () < 7) { // not in column h
      Square destination = new Square (square.getRow () - 1, square.getColumn () + 1);
      if (differentColour (board, square, destination)) {
        moves.add (destination);
      }
    }

    return moves;
  }

  // compute possible knight moves
  // -- assume square is position of a knight 
  private List<Square> findKnightMoves (Scene board, Square square) {
    List<Square> moves = new ArrayList<Square> ();

    if (square.getRow () < 6) { // not rows 7 or 8
      if (square.getColumn () > 0) { // not column a
        addValidMove (board, square, new Square (square.getRow () + 2, square.getColumn () - 1), moves);
      }
      if (square.getColumn () < 7) { // not column h
        addValidMove (board, square, new Square (square.getRow () + 2, square.getColumn () + 1), moves);
      }
    }

    if (square.getRow () > 1) { // not rows 1 or 2
      if (square.getColumn () > 0) { // not column a
        addValidMove (board, square, new Square (square.getRow () - 2, square.getColumn () - 1), moves);
      }
      if (square.getColumn () < 7) { // not column h
        addValidMove (board, square, new Square (square.getRow () - 2, square.getColumn () + 1), moves);
      }
    }

    if (square.getColumn () > 1) { // not columns a or b
      if (square.getRow () > 0) { // not row 1
        addValidMove (board, square, new Square (square.getRow () - 1, square.getColumn () - 2), moves);
      }
      if (square.getRow () < 7) { // not row 8
        addValidMove (board, square, new Square (square.getRow () + 1, square.getColumn () - 2), moves);
      }
    }

    if (square.getColumn () < 6) { // not columns g or h
      if (square.getRow () > 0) { // not row 1
        addValidMove (board, square, new Square (square.getRow () - 1, square.getColumn () + 2), moves);
      }
      if (square.getRow () < 7) { // not row 8
        addValidMove (board, square, new Square (square.getRow () + 1, square.getColumn () + 2), moves);
      }
    }

    return moves;
  }

  // compute possible king moves
  // -- assume square is position of a king
  // -- does not check if move is to an undefended square
  private List<Square> findKingMoves (Scene board, Square square) {
    List<Square> moves = new ArrayList<Square> ();

    if (square.getRow () > 0) { // not in row 8
      addValidMove (board, square, new Square (square.getRow () - 1, square.getColumn ()), moves);
    }
    if (square.getRow () < 7) { // not in row 1
      addValidMove (board, square, new Square (square.getRow () + 1, square.getColumn ()), moves);
    }
    if (square.getColumn () > 0) { // not in column 1
      addValidMove (board, square, new Square (square.getRow (), square.getColumn () - 1), moves);
    }
    if (square.getColumn () < 7) { // not in column 8
      addValidMove (board, square, new Square (square.getRow (), square.getColumn () + 1), moves);
    }
    if (square.getRow () > 0 && square.getColumn () > 0) { // not in row 8 or column 1
      addValidMove (board, square, new Square (square.getRow () - 1, square.getColumn () - 1), moves);
    }
    if (square.getRow () > 0 && square.getColumn () < 7) { // not in row 8 or column 8
      addValidMove (board, square, new Square (square.getRow () - 1, square.getColumn () + 1), moves);
    }
    if (square.getRow () < 7 && square.getColumn () > 0) { // not in row 1 or column 1
      addValidMove (board, square, new Square (square.getRow () + 1, square.getColumn () - 1), moves);
    }
    if (square.getRow () < 7 && square.getColumn () < 7) { // not in row 8 or column 8
      addValidMove (board, square, new Square (square.getRow () + 1, square.getColumn () + 1), moves);
    }

    return moves;
  }

  // compute possible queen moves
  // -- assume square is position of a queen
  private List<Square> findQueenMoves (Scene board, Square square) {
    List<Square> moves = new ArrayList<Square> ();

    // moves upwards
    int tryRow = square.getRow () - 1;
    boolean metPiece = false;
    while (!metPiece && tryRow >= 0) {
      Square destination = new Square (tryRow, square.getColumn ());
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow -= 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves downwards
    tryRow = square.getRow () + 1;
    metPiece = false;
    while (!metPiece && tryRow <= 7) {
      Square destination = new Square (tryRow, square.getColumn ());
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow += 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves left
    int tryCol = square.getColumn () - 1;
    metPiece = false;
    while (!metPiece && tryCol >= 0) {
      Square destination = new Square (square.getRow (), tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryCol -= 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves right
    tryCol = square.getColumn () + 1;
    metPiece = false;
    while (!metPiece && tryCol <= 7) {
      Square destination = new Square (square.getRow (), tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryCol += 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        } 
      }
    }

    // moves up and left
    tryRow = square.getRow () - 1;
    tryCol = square.getColumn () - 1;
    metPiece = false;
    while (!metPiece && tryRow >= 0 && tryCol >= 0) {
      Square destination = new Square (tryRow, tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow -= 1;
        tryCol -= 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves down and left
    tryRow = square.getRow () + 1;
    tryCol = square.getColumn () - 1;
    metPiece = false;
    while (!metPiece && tryRow <= 7 && tryCol >= 0) {
      Square destination = new Square (tryRow, tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow += 1;
        tryCol -= 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves up and right
    tryRow = square.getRow () - 1;
    tryCol = square.getColumn () + 1;
    metPiece = false;
    while (!metPiece && tryRow >= 0 && tryCol <= 7) {
      Square destination = new Square (tryRow, tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow -= 1;
        tryCol += 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves down and right
    tryRow = square.getRow () + 1;
    tryCol = square.getColumn () + 1;
    metPiece = false;
    while (!metPiece && tryRow <= 7 && tryCol <= 7) {
      Square destination = new Square (tryRow, tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow += 1;
        tryCol += 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        } 
      }
    }

    return moves;
  }

  // compute possible rook moves
  // -- assume square is position of a rook
  private List<Square> findRookMoves (Scene board, Square square) {
    List<Square> moves = new ArrayList<Square> ();

    // moves upwards
    int tryRow = square.getRow () - 1;
    boolean metPiece = false;
    while (!metPiece && tryRow >= 0) {
      Square destination = new Square (tryRow, square.getColumn ());
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow -= 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves downwards
    tryRow = square.getRow () + 1;
    metPiece = false;
    while (!metPiece && tryRow <= 7) {
      Square destination = new Square (tryRow, square.getColumn ());
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow += 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves left
    int tryCol = square.getColumn () - 1;
    metPiece = false;
    while (!metPiece && tryCol >= 0) {
      Square destination = new Square (square.getRow (), tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryCol -= 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves right
    tryCol = square.getColumn () + 1;
    metPiece = false;
    while (!metPiece && tryCol <= 7) {
      Square destination = new Square (square.getRow (), tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryCol += 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        } 
      }
    }

    return moves;
  }

  // compute possible bishop moves
  // -- assume square is location of a bishop
  private List<Square> findBishopMoves (Scene board, Square square) {
    List<Square> moves = new ArrayList<Square> ();
    
    // moves up and left
    int tryRow = square.getRow () - 1;
    int tryCol = square.getColumn () - 1;
    boolean metPiece = false;
    while (!metPiece && tryRow >= 0 && tryCol >= 0) {
      Square destination = new Square (tryRow, tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow -= 1;
        tryCol -= 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves down and left
    tryRow = square.getRow () + 1;
    tryCol = square.getColumn () - 1;
    metPiece = false;
    while (!metPiece && tryRow <= 7 && tryCol >= 0) {
      Square destination = new Square (tryRow, tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow += 1;
        tryCol -= 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves up and right
    tryRow = square.getRow () - 1;
    tryCol = square.getColumn () + 1;
    metPiece = false;
    while (!metPiece && tryRow >= 0 && tryCol <= 7) {
      Square destination = new Square (tryRow, tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow -= 1;
        tryCol += 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        }
      }
    }

    // moves down and right
    tryRow = square.getRow () + 1;
    tryCol = square.getColumn () + 1;
    metPiece = false;
    while (!metPiece && tryRow <= 7 && tryCol <= 7) {
      Square destination = new Square (tryRow, tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow += 1;
        tryCol += 1;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        } 
      }
    }

    return moves;
  }

  /**
   * Calculate a list of possible destination squares for a piece in a scene.
   */
  public List<Square> proposeMovementFixations (Scene board, Square square) {
    String piece = board.getItem (square.getRow (), square.getColumn ());

    if (piece.equals ("P")) {
      return findWhitePawnMoves (board, square);
    } else if (piece.equals ("p")) {
      return findBlackPawnMoves (board, square);
    } else if (piece.equalsIgnoreCase ("N")) {
      return findKnightMoves (board, square);
    } else if (piece.equalsIgnoreCase ("K")) {
      return findKingMoves (board, square);
    } else if (piece.equalsIgnoreCase ("Q")) {
      return findQueenMoves (board, square);
    } else if (piece.equalsIgnoreCase ("R")) {
      return findRookMoves (board, square);
    } else if (piece.equalsIgnoreCase ("B")) {
      return findBishopMoves (board, square);
    } else {
      return new ArrayList<Square> (); // no moves
    }
  }
}

