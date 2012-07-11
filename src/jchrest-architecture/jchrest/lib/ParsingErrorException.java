// Copyright (c) 2012, Peter C. R. Lane
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.lib;

/** 
 * Exception to signal an error when reading in a model definition file.
 */
public class ParsingErrorException extends Exception {
  private String _message;

  public ParsingErrorException () {
    _message = "UNKNOWN";
  }

  public ParsingErrorException (String message) {
    _message = message;
  }

  public String getMessage () {
    return _message;
  }
};
