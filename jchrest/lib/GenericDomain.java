package jchrest.lib;

/**
  * The GenericDomain is used when no domain-specific methods have been created.
  */
public class GenericDomain implements DomainSpecifics {
  public ListPattern normalise (ListPattern pattern) {
    return pattern;
  }
}
