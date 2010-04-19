package jchrest.lib;

/** 
 * The PrimitivePattern is an abstract class for a group of pattern 
 * types which can be used within a compound pattern.  
 *
 * @author Peter C. R. Lane
 */
public abstract class PrimitivePattern extends Pattern {
  abstract public boolean equalPrimitive (PrimitivePattern pattern);
}
