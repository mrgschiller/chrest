package jchrest.gui;

import java.awt.Color;
import java.awt.Graphics2D;
import java.util.List;

/** Methods required for a node or link to be displayed within the ltm network.
  *
  * @author Peter C. R. Lane
  */
public interface LtmTreeViewNode {
	public List<LtmTreeViewNode> getChildren ();
	public int getWidth (Graphics2D g2, Size size);
	public int getHeight (Graphics2D g2, Size size);
	public void draw (Graphics2D g2, int x, int y, int w, int h, Size size);
	public boolean isRoot ();
  public void add (LtmTreeViewNode node);
}

