package jchrest.lib;

/**
 * Enumerated type to manage the fixation type.
 */
public enum FixationType {
       none ("No heuristic"),
       ltm ("LTM heuristic"),
       randomItem ("Random item heuristic"),
       randomPlace ("Random place heuristic"),
       proposedMove ("Proposed movement heuristic") // TODO: this needs a better name
         ;

  private FixationType (String description) {
    _description = description;
  }

  private final String _description;

  public String toString () {
    return _description;
  }
}

