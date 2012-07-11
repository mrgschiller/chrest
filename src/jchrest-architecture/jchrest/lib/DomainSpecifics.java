// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.lib;

import java.util.List;

/**
  * An interface for defining domain-specific methods.
  */
public interface DomainSpecifics {
  public ListPattern normalise (ListPattern pattern);
  public List<Square> proposeMovementFixations (Scene scene, Square square);
}

