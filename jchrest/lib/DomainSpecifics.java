package jchrest.lib;

import java.util.List;

/**
  * An interface for defining domain-specific methods.
  */
public interface DomainSpecifics {
  public ListPattern normalise (ListPattern pattern);
  public List<Square> proposeMovementFixations (Scene scene, Square square);
}

