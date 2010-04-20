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

        // P p K k B b N n Q q R r
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
}

