package jchrest.gui;

import jchrest.architecture.*;
import jchrest.lib.Pattern;

import java.awt.*;
import java.awt.BorderLayout;
import java.awt.event.*;
import java.awt.font.TextLayout;
import java.awt.geom.Rectangle2D;
import java.awt.image.*;
import java.io.*;
import java.util.ArrayList;
import java.util.List;
import javax.imageio.*;
import javax.swing.*;
import javax.swing.border.*;

/**
 * This panel displays the model long-term memory within a tree view, 
 * supporting some interactions to alter the display.
 */
public class ChrestLtmView extends JPanel {
  private Chrest _model;
  private TreeViewPane _ltmView;

  public ChrestLtmView (Chrest model) {
    super ();

    _model = model;
    setBorder (new TitledBorder ("LTM"));
    setLayout (new BorderLayout ());

    // -- the treeview pane
    if (_model.getTotalLtmNodes () > 5000) {
      _ltmView = null;
      add (new JLabel ("Sorry - LTM too large to display"));
    } else {
      _ltmView = new TreeViewPane (new TreeViewNode (new NodeDisplay (null)));
      _constructingTreeThread = new ConstructTreeThread ();
      _constructingTreeThread.execute ();
      add (new JScrollPane (_ltmView));
    }

    add (createToolBar (), BorderLayout.SOUTH);
  }

  private ConstructTreeThread _constructingTreeThread;

  private class ConstructTreeThread extends SwingWorker<Void, Void> {
    LtmTreeViewNode _newTree;
    
    @Override
      public Void doInBackground () {
        _newTree = constructTree ();
        return null;
      }

    @Override
      protected void done () {
        _ltmView.changeRoot (new TreeViewNode (_newTree));
        _constructingTreeThread = null;
      }
  }

  public void update () {
    if (_ltmView != null) {
      if (_model.getTotalLtmNodes () > 5000) {
        // TODO : change display if number of nodes is too large
        //        this.add (new JLabel ("Sorry - LTM too large to display"));
      } else {
        _constructingTreeThread = new ConstructTreeThread ();
        _constructingTreeThread.execute ();
      }
    }
  }

  private JComboBox createOrientationBox () {
    JComboBox box = new JComboBox (new String[] { "Horizontal", "Vertical" });
    box.setSelectedIndex (0); // Display begins in horizontal orientation
    box.addActionListener (new ActionListener () {
      public void actionPerformed (ActionEvent e) {
        JComboBox box = (JComboBox)(e.getSource ());
        updateOrientation (box.getSelectedIndex () == 0 ? 
          Orientation.HORIZONTAL :
          Orientation.VERTICAL);
      }
    });

    return box;
  }

	private JComboBox createSizeBox () {
		JComboBox box = new JComboBox ();
		for (Size size : Size.getValues ()) {
			box.addItem (size.toString ());
		}
		box.setSelectedIndex (1); // Display begins in Medium size
		box.addActionListener (new ActionListener () {
			public void actionPerformed (ActionEvent e) {
				JComboBox box = (JComboBox)(e.getSource ());
				updateSize (Size.getValues().get (box.getSelectedIndex ()));
			}
		});
		return box;
	}
	
	private JToolBar createToolBar () {
		JToolBar tools = new JToolBar ();

		tools.add (new JLabel ("Orientation: "));
		tools.add (createOrientationBox ());
		tools.addSeparator();
		tools.add (new JLabel ("Size: "));
		tools.add (createSizeBox ());

		return tools;
	}

	public void setStandardDisplay () {
    if (_ltmView != null) {
      updateOrientation (Orientation.HORIZONTAL);
      updateSize (Size.getValues().get (1));
    }
	}

  /**
   * Relayout and draw the treeview nodes.
   */
	public void drawTreeView () {
    if (_ltmView != null) {
      _ltmView.relayout();
    }
	}

  /**
   * Save the network as an image file.  Currently, only .png format is supported.
   */
  public void saveLongTermMemory (File file) {
    BufferedImage img = new BufferedImage(_ltmView.getExtentWidth (), _ltmView.getExtentHeight (), BufferedImage.TYPE_INT_RGB);  
    img.createGraphics();  
    _ltmView.paint (img.getGraphics());
    try {
      String format = "png"; // TODO Extend range  
      ImageIO.write(img, format, file);
    } catch (IOException e) {  
      JOptionPane.showMessageDialog (this, 
          "Failed to write image",
          "Failed to write image",
          JOptionPane.ERROR_MESSAGE);
    }  
  }

  /**
   * Change the orientation of the displayed network.
   */
	public void updateOrientation (Orientation newOrientation) {
		_ltmView.setOrientation (newOrientation);
	}
	
  /**
   * Change the displayed size of the network.
   */
	public void updateSize (Size newSize) {
		_ltmView.setSize (newSize);
	}

  /**
   * Wrap the model's LTM as a set of LtmTreeVewNode objects,
   * joining the three types of LTM into a single tree.
   */
  private LtmTreeViewNode constructTree () {
    LtmTreeViewNode baseTreeViewNode = new NodeDisplay (null);
    baseTreeViewNode.add (constructTree (_model.getLtmByModality(Pattern.makeVisualList (new String[]{})), 3));
    baseTreeViewNode.add (constructTree (_model.getLtmByModality(Pattern.makeVerbalList (new String[]{})), 3));
    baseTreeViewNode.add (constructTree (_model.getLtmByModality(Pattern.makeActionList (new String[]{})), 3));
    return baseTreeViewNode;
  }

  /** 
   * Wrap the model's LTM as a set of LtmTreeViewNode objects.
   */
  private LtmTreeViewNode constructTree (Node baseNode, int remainingDepth) {
    LtmTreeViewNode baseTreeViewNode = new NodeDisplay (baseNode);
    if (remainingDepth >= 0) { // add children unless already at maximum depth
      for (Link link : baseNode.getChildren ()) {
        LtmTreeViewNode linkNode = new LinkDisplay (link);
        linkNode.add (constructTree (link.getChildNode (), remainingDepth-1));
        baseTreeViewNode.add (linkNode);
      }
    }

    return baseTreeViewNode;
  }
}

enum Orientation { HORIZONTAL, VERTICAL }

/**
 * Display a link within the LTM view.  Note that a link can 
 * never be a root node.
 */
class LinkDisplay implements LtmTreeViewNode {
  private Link _link;
  private List<LtmTreeViewNode> _children;

  public LinkDisplay (Link link) {
    _link = link;
    _children = new ArrayList<LtmTreeViewNode> ();
  }

  public List<LtmTreeViewNode> getChildren () {
    return _children;
  }

  public int getWidth (Graphics2D g, Size size) {
    int width =  2 * size.getMargin ();

    if ( size.isSmall () ) {
      width = size.getSmallSize ();
    } else {
      width += size.getWidth (_link.getTest().toString (), g);
    }

    return width;
  }

  public int getHeight (Graphics2D g, Size size) {
    int height = 2 * size.getMargin ();

    if ( size.isSmall () ) {
      height = size.getSmallSize ();
    } else {
      height += size.getHeight (_link.getTest().toString (), g);
    }
    return height;
  }

  public void draw (Graphics2D g, int x, int y, int w, int h, Size size) {
    if ( size.isSmall () ) {
      drawSmallNode (g, x, y, w, h);
    } else {
      drawRegularNode (g, x, y, w, h, size);
    }
  }

  public boolean isRoot () {
    return false;
  }

  public void add (LtmTreeViewNode node) {
    _children.add (node);
  }

  private void drawSmallNode (Graphics2D g, int x, int y, int w, int h) {
    g.setColor (Color.LIGHT_GRAY);
    g.fillRect (x, y, w, h);
  }

  private void drawRegularNode (Graphics2D g, int x, int y, int w, int h, Size size) {
    g.setBackground (Color.LIGHT_GRAY);
    g.clearRect (x+1, y+1, w-1, h-1);
    g.setColor (Color.BLACK);

    size.drawText (g, x, y, _link.getTest().toString ());
  }
}

/** Display a given TreeViewNode with a JPanel.  */
class TreeViewPane extends JPanel {
	private final static long serialVersionUID = 2;
	
	private int _maxX;
	private int _maxY;
	private TreeViewNode _rootnode;
	private Orientation _orientation;
	private Size _size;

	public TreeViewPane (TreeViewNode rootNode) {
		super();
		
		setBackground(Color.white);

		_rootnode = rootNode;
		_orientation = Orientation.HORIZONTAL;
		_size = Size.getValues().get (1);

		// give some preferred size to get us started
		_maxX = 100;
		_maxY = 100;
		setPreferredSize (new Dimension (_maxX, _maxY));
	}

	public void setOrientation (Orientation newOrientation) {
		_orientation = newOrientation;
		relayout();
	}

	public void setSize (Size newSize) {
		_size = newSize;
		relayout();
	}
	
	public void changeRoot (TreeViewNode newRoot) {
		_rootnode = newRoot;
		relayout();
	}

  private final int MAXDEPTH = 7; // maximum levels of nodes of network

	public void relayout () {
		_rootnode.layoutNode (getGraphics (), 10, 10, _orientation, _size, MAXDEPTH);
		_maxX = 20 + _rootnode.getExtentWidth (getGraphics (), _orientation, _size);
		_maxY = 20 + _rootnode.getExtentHeight (getGraphics (), _orientation, _size);
		setPreferredSize (new Dimension (_maxX, _maxY));
		// notify parent container (which is probably a JScrollPane) to update itself
		if (getParent () != null) ((JComponent)getParent ()).updateUI ();
		repaint ();
	}

  public void paint(Graphics g) {
    ((Graphics2D)g).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    super.paint (g); // make sure the background of the JPanel is drawn
		Graphics2D g2 = (Graphics2D)g;

		g2.setBackground (Color.WHITE);
		g2.clearRect (0, 0, this.getExtentWidth (), this.getExtentHeight ());
		_rootnode.drawNode (g, _size, _orientation);
	}

  int getExtentWidth () {
    return 20 + _rootnode.getExtentWidth (getGraphics (), _orientation, _size);
  }

  int getExtentHeight () {
    return 20 + _rootnode.getExtentHeight (getGraphics (), _orientation, _size);
  }
}

/** The TreeViewNode is a wrapper around a Node, 
 * managing the layout and display of the node and its children.
 */
class TreeViewNode {
	int _x;  // x position on the canvas
	int _y;  // y position 
	int _w;  // the width
	int _h;  // the height
	// the extent of a node is the amount of space taken up by itself
	// and its children
	int _extentWidth;
	int _extentHeight;
	// hold a pointer to the object being displayed
	LtmTreeViewNode _object;
	ArrayList<TreeViewNode> _children;
	
	public TreeViewNode (LtmTreeViewNode object) {
		_object = object;
		_children = new ArrayList<TreeViewNode> ();
		// work through any children of node, turning them into TreeViewNodes
		for (LtmTreeViewNode child : _object.getChildren ()) {
			_children.add (new TreeViewNode (child));
		}
		
		setDefaults();
	}

	private void setDefaults () {
		// the position and size of the node will be created during layout
		_x = 0;
		_y = 0;
  	_w = 0; 
		_h = 0;
		_extentWidth = 0;
		_extentHeight = 0;
	}

	// return true/false depending if this node has children
	private boolean hasChildren () { return (!(_children.isEmpty ())); }

	/** Return the visible extent of this node as a Rectangle */
	public Rectangle getVisibleExtent () {
		return new Rectangle(_x, _y, _w, _h);
	}

	/** Compute the extent in width of a TreeViewNode.  Different routines compute width
	  * depending on direction of layout.
	  */
	public int getExtentWidth (Graphics g, Orientation orientation, Size size) {
		// return cached value, if present
		if (_extentWidth != 0) return _extentWidth;

		if (orientation == Orientation.HORIZONTAL) {
			_extentWidth = getExtentWidthHorizontalLayout (g, orientation, size);
		} else {
			_extentWidth = getExtentWidthVerticalLayout (g, orientation, size);
		}
		return _extentWidth;
	}

	/** Compute the extent in height of a TreeViewNode.  Different routines compute height 
	  * depending on direction of layout.
	  */
	public int getExtentHeight (Graphics g, Orientation orientation, Size size) {
		// return a cached value, if present
		if (_extentHeight != 0) return _extentHeight;

		if (orientation == Orientation.HORIZONTAL) {
			_extentHeight = getExtentHeightHorizontalLayout (g, orientation, size);
		} else {
			_extentHeight = getExtentHeightVerticalLayout (g, orientation, size);
		}
		return _extentHeight;
	}

	// for horizontal layout, extent is width of widest child
	private int getExtentWidthHorizontalLayout (Graphics g, Orientation orientation, Size size) {
		int width = _object.getWidth ((Graphics2D)g, size);
		if (hasChildren ()) {
			int maxChildWidth = 0;

			for (TreeViewNode child : _children) {
				if (maxChildWidth < child.getExtentWidth (g, orientation, size)) {
					maxChildWidth = child.getExtentWidth (g, orientation, size);
				}
			}

			width += size.getHorizontalSeparator (orientation) + maxChildWidth;
		}

		return width;
	}

	// for vertical layout, add all the widths of the children and gaps to get extent
	private int getExtentWidthVerticalLayout (Graphics g, Orientation orientation, Size size) {
		int totalChildWidth = 0;
		if (hasChildren ()) {
			for (TreeViewNode child : _children) {
				totalChildWidth += child.getExtentWidth (g, orientation, size);
			}
			// add n-1 gaps
			totalChildWidth += size.getHorizontalSeparator (orientation) *
					(_children.size () - 1);
		}
		return Math.max (totalChildWidth, _object.getWidth ((Graphics2D)g, size));
	}

	// for horizontal layout, add all the heights of the children and gaps to get extent
	private int getExtentHeightHorizontalLayout (Graphics g, Orientation orientation, Size size) {
		int totalChildHeight = 0;
		if (hasChildren ()) {
			for (TreeViewNode child : _children) {
				totalChildHeight += child.getExtentHeight (g, orientation, size);
			}
			// add n-1 gaps
			totalChildHeight += size.getVerticalSeparator (orientation) *
				(_children.size () - 1);
		}
		return Math.max (totalChildHeight, _object.getHeight ((Graphics2D)g, size));
	}

	// for horizontal layout, extent is height of tallest child
	private int getExtentHeightVerticalLayout (Graphics g, Orientation orientation, Size size) {
		int height = _object.getHeight((Graphics2D)g, size);
		if (hasChildren ()) {
			int maxChildHeight = 0;

			for (TreeViewNode child : _children) {
				if (maxChildHeight < child.getExtentHeight (g, orientation, size)) {
					maxChildHeight = child.getExtentHeight (g, orientation, size);
				}
			}	

			height += size.getVerticalSeparator (orientation) + maxChildHeight; 
		}

		return height;
	}

	// layoutNode assigns new x and y values to the node, respecting new orientation and size
	public void layoutNode (Graphics g, int x, int y, Orientation orientation, Size size, int remainingDepth) {
		_x = x;
		_y = y;
		_w = _object.getWidth((Graphics2D)g, size);
		_h = _object.getHeight((Graphics2D)g, size);

		// clear cached values, to force recomputation
		_extentWidth = 0;
		_extentHeight = 0;
		if (orientation == Orientation.HORIZONTAL) {
			layoutChildrenHorizontally (g, orientation, size, remainingDepth);
		} else {
			layoutChildrenVertically (g, orientation, size, remainingDepth);
		}
	}

	// horizontal layout means children displayed vertically
	private void layoutChildrenHorizontally (Graphics g, Orientation orientation, Size size, int remainingDepth) {
    if (remainingDepth < 0) return; // reached cutoff point for depth of network, so add no more children
		if (!hasChildren ()) return ; // nothing to do, if no children
		int nextY = _y;
		// each child's x position is to the right of this node
		int thisX = _x + size.getHorizontalSeparator(orientation) + _w;
		// nextY is incremented by child's height + verticalSeparator
		//       to get the vertical position of the next child
		//       to get the vertical position of the next child
		for (TreeViewNode child : _children) {
			child.layoutNode(g, thisX, nextY, orientation, size, remainingDepth-1);
			nextY += size.getVerticalSeparator (orientation);
			nextY += child.getExtentHeight(g, orientation, size);
		}
		// move the node DOWN to center it
		_y += 0.5*(getExtentHeight(g, orientation, size) - _h);
	}

	// vertical layout means children displayed horizontally
	private void layoutChildrenVertically (Graphics g, Orientation orientation, Size size, int remainingDepth) {
    if (remainingDepth < 0) return; // reached cutoff point for depth of network, so add no more children
		if (!hasChildren ()) return; // nothing to do, if no children
		int thisY = _y + size.getVerticalSeparator(orientation) + _h;
		int nextX = _x;
		// nextX is incremented by child's width + horizontalSeparator
		//       to get horizontal position of the next child
		for (TreeViewNode child : _children) {
			child.layoutNode(g, nextX, thisY, orientation, size, remainingDepth-1);
			nextX += size.getHorizontalSeparator(orientation);
			nextX += child.getExtentWidth(g, orientation, size);
		}
		// move the node RIGHT to center it
		_x += 0.5 * (getExtentWidth(g, orientation, size) - _w);
	}

	public int getLeftX () { return _x; }
	public int getMidX () { return _x + (Math.round (_w / 2)); }
	public int getRightX () { return _x + _w; }
	public int getTopY () { return _y; }
	public int getMidY () { return _y + (Math.round (_h / 2)); }
	public int getBottomY () { return _y + _h; }

	/** A TreeViewNode draws itself by requesting its object to draw 
	  * itself within the specified area, on the given context.
	  */
	public void drawNode (Graphics g, Size size, Orientation orientation) {
		Graphics2D g2 = (Graphics2D)g;
		drawLinks (g2, orientation); // draw the links first, so objects overwrite them
		drawNodeBorder (g2);
		_object.draw (g2, _x, _y, _w, _h, size);
		drawChildren (g, size, orientation);
	}

	private void drawNodeBorder (Graphics2D g2) {
		if (_object.isRoot ()) return; // allow rootnode to be drawn differently
		g2.clearRect (_x, _y, _w, _h);
		g2.setColor (Color.BLACK);
    g2.drawRect (_x, _y, _w, _h);
	}

	private void drawChildren (Graphics g, Size size, Orientation orientation) {
		if (hasChildren ()) {
			for (TreeViewNode child : _children) {
				child.drawNode (g, size, orientation);
			}
		}
	}

	private void drawLinks (Graphics2D g2, Orientation orientation) {
		if (!hasChildren ()) return; // nothing to do if no children
		g2.setColor(Color.black);
		
		if (orientation == Orientation.HORIZONTAL) {
			drawHorizontalLinks (g2, getFromX (orientation), getFromY (orientation));
		} else {
			drawVerticalLinks (g2, getFromX (orientation), getFromY (orientation));
		}
	}

	private int getFromX (Orientation orientation) {
		if (_object.isRoot () || orientation == Orientation.VERTICAL) {
			return getMidX ();
		} else {
			return getRightX ();
		}
	}

	private int getFromY (Orientation orientation) {
		if (_object.isRoot () || orientation == Orientation.HORIZONTAL) {
			return getMidY ();
		} else {
			return getBottomY ();
		}
	}
	
	private void drawHorizontalLinks (Graphics2D g2, int from_x, int from_y) {
		for (TreeViewNode child : _children) {
			g2.drawLine (from_x, from_y, child.getLeftX (), child.getMidY ());
		}
	}

	private void drawVerticalLinks (Graphics2D g2, int from_x, int from_y) {
		for (TreeViewNode child : _children) {
			g2.drawLine (from_x, from_y, child.getMidX (), child.getTopY ());
		}
	}
}

