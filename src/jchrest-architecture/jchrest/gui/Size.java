package jchrest.gui;

import java.awt.*;
import java.awt.font.*;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

public class Size {
	private final String _name;
	private final Font _font;
	private final int _margin;
	private final int _axis_gap_1; // gap between nodes in one direction
	private final int _axis_gap_2; // gap between nodes in other direction
	private final int _small_size; // size of box if _margin == 0
	
	private static List<Size> _values = null;

	Size (String name, Font font, int margin, int axis_gap_1, int axis_gap_2, int small_size) {
		_name = name;
		_font = font;
		_margin = margin;
		_axis_gap_1 = axis_gap_1;
		_axis_gap_2 = axis_gap_2;
		_small_size = small_size;
	}

	public String toString () { return _name; }
	public Font getFont () { return _font; }
	public int getMargin () { return _margin; }
	public int getHorizontalSeparator (Orientation orientation) {
		return ((orientation == Orientation.HORIZONTAL) ? _axis_gap_1 : _axis_gap_2);
	}

	public int getVerticalSeparator (Orientation orientation) {
		return ((orientation == Orientation.HORIZONTAL) ? _axis_gap_1 : _axis_gap_2);
	}
	/** Return true if this size should be drawn as a box rather than with text and graphics.
	 Indicator is whether a margin has been provided or not. */
	public boolean isSmall () { return _margin == 0; }
	/** Size of box for small size. */
	public int getSmallSize () { return _small_size; }

	public static List<Size> getValues () { 
		if (_values == null) {
			_values = new ArrayList<Size> ();
			_values.add (new Size ("Large Text", new Font ("Times", Font.PLAIN, 20), 7, 30, 12, 0));
			_values.add (new Size ("Medium Text", new Font ("Times", Font.PLAIN, 13), 4, 15, 7, 0));
			_values.add (new Size ("Small Text", new Font ("Times", Font.PLAIN, 9), 2, 8, 5, 0));
			_values.add (new Size ("Small-10", null, 0, 7, 7, 10));
			_values.add (new Size ("Small-5", null, 0, 5, 5, 5));
			_values.add (new Size ("Small-2", null, 0, 2, 2, 2));
		}
		
		return _values; 
	}

	/** Return the TextLayout for given string at this size */
	public TextLayout getTextLayout (String str, Graphics2D g) {
		FontRenderContext frc = g.getFontRenderContext ();
		return new TextLayout (str, getFont (), frc);
	}
	
	/** Return the bounding box for given string drawn at this size */
	public Rectangle2D getTextBounds (String str, Graphics2D g) {
		return getTextLayout(str, g).getBounds ();
	}

	public int getHeight (String str, Graphics2D g) {
		return (int)(getTextBounds(str, g).getHeight ());
	}

	public int getWidth (String str, Graphics2D g) {
		return (int)(getTextBounds(str, g).getWidth ());
	}
	
	/** Display given text at coordinates. 
	 * x, y represent the top left corner.  Add the desired margin. 
	 */
	public void drawText(Graphics2D g, float x, float y, String str) {
		float m = (float)(getMargin ());
		TextLayout layout = getTextLayout (str, g);
		layout.draw (g, x + m, y + getHeight (str, g) + m);
	}

}

