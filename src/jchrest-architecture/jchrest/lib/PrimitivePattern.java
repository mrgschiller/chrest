package jchrest.lib;

/** 
 * The PrimitivePattern is an abstract class for a group of pattern 
 * types which can be used within a compound pattern.  
 *
 * @author Peter C. R. Lane
 */
public abstract class PrimitivePattern extends Pattern {

  /**
   * Provide a method to test for equality between PrimitivePattern instances.
   */
  abstract public boolean equalPrimitive (PrimitivePattern pattern);
}
