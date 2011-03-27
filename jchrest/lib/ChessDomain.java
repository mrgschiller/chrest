package jchrest.lib;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
  * The ChessDomain is used for chess modelling.
  */
public class ChessDomain implements DomainSpecifics {

  // map stores the canonical order of the chess pieces
  private static Map<String, Integer> pieceOrder;
  static {
    pieceOrder = new HashMap<String, Integer> ();
    pieceOrder.put("P", 0);
    pieceOrder.put("p", 1);
    pieceOrder.put("K", 2);
    pieceOrder.put("k", 3);
    pieceOrder.put("B", 4);
    pieceOrder.put("b", 5);
    pieceOrder.put("N", 6);
    pieceOrder.put("b", 7);
    pieceOrder.put("Q", 8);
    pieceOrder.put("q", 9);
    pieceOrder.put("R", 10);
    pieceOrder.put("r", 11);
  }

  /**
   * Sort given list pattern into a canonical order of chess pieces, as 
   * defined in deGroot and Gobet (1996).
   * The order is:  P p K k B b N n Q q R r 
   * If the pieces are the same, then order is based on column, and then on row.
   */
  public ListPattern normalise (ListPattern pattern) {
    return pattern.sort (new Comparator<PrimitivePattern> () {
      public int compare (PrimitivePattern left, PrimitivePattern right) {
        assert (left instanceof ItemSquarePattern);
        assert (right instanceof ItemSquarePattern);
        ItemSquarePattern leftIos = (ItemSquarePattern)left;
        ItemSquarePattern rightIos = (ItemSquarePattern)right;

        // check item
        if (pieceOrder.get (leftIos.getItem()) < pieceOrder.get (rightIos.getItem ())) return -1;
        if (pieceOrder.get (leftIos.getItem()) > pieceOrder.get (rightIos.getItem ())) return 1;
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
   * Construct a chess board given a string definition.
   */
  public static Scene constructBoard (String definition) {
    Scene board = new Scene ("chess-board", 8, 8);

    for (int row = 0; row < 8; ++row) {
      for (int col = 0; col < 8; ++col) {
        String piece = definition.substring (col + 9*row, 1 + col + 9*row);
        board.setItem (row, col, piece);
      }
    }

    return board;
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

    lineMove (board, moves, square, -1, 0); // moves upwards
    lineMove (board, moves, square, 1, 0); // moves down
    lineMove (board, moves, square, 0, -1); // moves left
    lineMove (board, moves, square, 0, +1); // moves right
    lineMove (board, moves, square, -1, -1); // moves up and left
    lineMove (board, moves, square, +1, -1); // moves down and left
    lineMove (board, moves, square, -1, +1); // moves up and right
    lineMove (board, moves, square, +1, +1); // moves down and right

    return moves;
  }

  // compute possible rook moves
  // -- assume square is position of a rook
  private List<Square> findRookMoves (Scene board, Square square) {
    List<Square> moves = new ArrayList<Square> ();

    lineMove (board, moves, square, -1, 0); // moves upwards
    lineMove (board, moves, square, 1, 0); // moves down
    lineMove (board, moves, square, 0, -1); // moves left
    lineMove (board, moves, square, 0, +1); // moves right

    return moves;
  }

  // compute possible bishop moves
  // -- assume square is location of a bishop
  private List<Square> findBishopMoves (Scene board, Square square) {
    List<Square> moves = new ArrayList<Square> ();
    
    lineMove (board, moves, square, -1, -1); // moves up and left
    lineMove (board, moves, square, +1, -1); // moves down and left
    lineMove (board, moves, square, -1, +1); // moves up and right
    lineMove (board, moves, square, +1, +1); // moves down and right

    return moves;
  }

  // move piece in direction given by deltas, until reach edge of board or a piece.
  // in case where piece reached is of different colour, include that piece's square.
  private void lineMove (Scene board, List<Square> moves, Square square, int rowDelta, int colDelta) {
    int tryRow = square.getRow () + rowDelta;
    int tryCol = square.getColumn () + colDelta;
    boolean metPiece = false;
    while (!metPiece && tryRow >=0 && tryRow <= 7 && tryCol >= 0 && tryCol <= 7) {
      Square destination = new Square (tryRow, tryCol);
      if (board.isEmpty (destination.getRow (), destination.getColumn ())) {
        moves.add (destination);
        tryRow += rowDelta;
        tryCol += colDelta;
      } else {
        metPiece = true;
        if (differentColour (board, square, destination)) {
          moves.add (destination);
        } 
      }
    }
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

