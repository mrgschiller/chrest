package jchrest.lib;

import java.awt.Component;
import java.io.*;
import java.lang.Character;
import javax.swing.*;

/** A collection of static methods used when handling files */
public class FileUtilities {
  // -- fields and methods to handle a file dialog
	private final static JFileChooser _fileChooser = new JFileChooser ();

  /**
   * Request user for a filename to save to.  Argument 'parent' centres the dialog on the 
   * given component.
   */
  public static File getSaveFilename (Component parent) {
    _fileChooser.setMultiSelectionEnabled (false);
    _fileChooser.setSelectedFile (new File ("x")); // clear the previous selection
    _fileChooser.setSelectedFile (new File (""));
    int option = _fileChooser.showSaveDialog (parent);
    if (option == JFileChooser.APPROVE_OPTION) {
      File filename = _fileChooser.getSelectedFile ();
      if (filename.exists()) {
        int overwrite = JOptionPane.showConfirmDialog(parent, 
            "File " + filename.getName() +
						" exists.  Are you sure you want to overwrite it?",
						"Warning: Overwriting file",
						JOptionPane.YES_NO_OPTION);
				if (overwrite == JOptionPane.YES_OPTION) {
					return filename;
				} else {
					return getSaveFilename (parent);
				}
			} else {
				return filename;
			}
		}
		return null;
	}

  /**
   * Get a filename to load from.
   */
  public static File getLoadFilename (Component parent) {
    _fileChooser.setMultiSelectionEnabled (false);
    _fileChooser.setSelectedFile (new File ("x")); // clear the previous selection
    _fileChooser.setSelectedFile (new File (""));
    int option = _fileChooser.showOpenDialog (parent);
    if (option == JFileChooser.APPROVE_OPTION) {
      File filename = _fileChooser.getSelectedFile ();
      if (filename.exists()) {
        return filename;
      } else {
        JOptionPane.showMessageDialog(parent, 
            "File " + filename.getName() +
            " does not exist.  You need to select an existing model.",
            "Error: No file exists",
            JOptionPane.ERROR_MESSAGE);
        return getLoadFilename (parent);
      }
		}
		return null;
	}

  // -- fields and methods for saving/loading data
	private static final int MAX_CHARS      = 1000; // arbitrary maximum size for read input
	private static final int MAX_MARK_CHARS = 1000; // arbitrary maximum size for marked chars
	private static final String 
		BEGIN_TAG_START = "<",
		END_TAG_START   = "</",		
		TAG_END         = ">";
	private static final char
		TAG_START_CHAR  = '<',
		TAG_END_CHAR    = '>';	   

	/** Read upto the 'until' char, but do not include it in the returned string */
	private static String readCharsFromFile (BufferedReader inputFile, char until, boolean includeLast)
		throws ParsingErrorException {

		char[] readChars = new char[MAX_CHARS];
		int c, charCount;
		charCount = 0;
		do {
			try {
				do { 
					inputFile.mark (MAX_MARK_CHARS);
					c = inputFile.read();
				} while ( (c == '\n') || (c == '\r') ); // ignore the newline characters
				readChars[charCount] = (char)c;
				++charCount;
			} catch (IOException ioe) {
				throw new ParsingErrorException ();
			}
			if (c == -1) { // hit EOF
				throw new ParsingErrorException ();
			}
		} while (c != until);
		
		// reset to before current char, if we are not including the last char
		if (!includeLast) { 
			try {
				inputFile.reset ();
			} catch (IOException ioe) {
				throw new ParsingErrorException ();
			}
		}

		return new String (readChars, 0, (includeLast ? charCount : charCount-1));
	}

	private static String readUntilChar (BufferedReader inputFile, char until)
		throws ParsingErrorException {
		
		return readCharsFromFile (inputFile, until, false);
	}

	private static String readUntilAndIncludingChar (BufferedReader inputFile, char until)
		throws ParsingErrorException {
		
		return readCharsFromFile (inputFile, until, true);
	}

	private static void ignoreUntilChar (BufferedReader inputFile, char until) 
		throws ParsingErrorException {
		
		readCharsFromFile (inputFile, until, false);
	}

	private static void ignoreUntilAndIncludingChar (BufferedReader inputFile, char until)
		throws ParsingErrorException {
		
		readCharsFromFile (inputFile, until, true);
	}

	private static void ignoreWhiteSpace (BufferedReader inputFile) {
		int c;
		try {
			do { 
				inputFile.mark (MAX_MARK_CHARS);
				c = inputFile.read ();
			} while ( (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r'));
		} catch (IOException ioe) {}

		try {
			inputFile.reset ();
		} catch (IOException ioe) {}
	}

	private static void acceptChars (BufferedReader inputFile, char[] chars) 
		throws ParsingErrorException {

		int c;
		for (int i = 0; i < chars.length; ++i) {
			try {
				do {
					c = inputFile.read();
				} while ( (c == '\n') || (c == '\r') ); // ignore the newlines
			} catch (IOException ioe) {
				throw new ParsingErrorException ();
			}
			if (Character.toLowerCase((char)c) != Character.toLowerCase(chars[i])) { // ignore case
				throw new ParsingErrorException ();
			}
		}
	}
	
	public static void acceptOpenTag (BufferedReader inputFile, String tagName) 
		throws ParsingErrorException {
		
		ignoreWhiteSpace (inputFile);
		acceptChars (inputFile, BEGIN_TAG_START.toCharArray ());
		acceptChars (inputFile, tagName.toCharArray());
		acceptChars (inputFile, TAG_END.toCharArray ());
	}

	public static void acceptCloseTag (BufferedReader inputFile, String tagName) 
		throws ParsingErrorException {
		
		ignoreWhiteSpace (inputFile);
		acceptChars (inputFile, END_TAG_START.toCharArray ());
		acceptChars (inputFile, tagName.toCharArray ());
		acceptChars (inputFile, TAG_END.toCharArray ());
	}

	public static boolean checkOpenTag (BufferedReader inputFile, String tagName) {
		boolean result = true;

		try {
			inputFile.mark(MAX_MARK_CHARS);
			acceptOpenTag (inputFile, tagName);
		} catch (IOException ioe) {
			result = false;
		} catch (ParsingErrorException pe) {
			result = false;
		}
		
		try {
			inputFile.reset ();
		} catch (IOException ioe) {
			result = false;
		}
		
		return result;
	}
	
	public static boolean checkCloseTag (BufferedReader inputFile, String tagName) {
		boolean result = true;

		try {
			inputFile.mark(MAX_MARK_CHARS);
			acceptCloseTag (inputFile, tagName);
		} catch (IOException ioe) {
			result = false;
		} catch (ParsingErrorException pe) {
			result = false;
		}
		
		try {
			inputFile.reset ();
		} catch (IOException ioe) {
			result = false;
		}
		
		return result;
	}

	public static String getString (BufferedReader inputFile) throws ParsingErrorException {
		return readUntilChar (inputFile, TAG_START_CHAR).trim(); // remove leading/trailing space
	}

	public static boolean getBoolean (BufferedReader inputFile) throws ParsingErrorException {
		return (java.lang.Boolean.valueOf (getString (inputFile))).booleanValue();
	}

	public static int getInt (BufferedReader inputFile) throws ParsingErrorException {
		int result;

		try {
			String data = readUntilChar (inputFile, TAG_START_CHAR);
			result = java.lang.Integer.parseInt(data);
		} catch (NumberFormatException e) {
			throw new ParsingErrorException ();
		}

		return result;
	}
/**
	public static ActionType readActionType (BufferedReader inputFile) 
		throws ParsingErrorException {
		
		ActionType item;
		
		if (checkOpenTag (inputFile, TextData.TEXT_DATA_TAG)) {
			item = TextData.readFromFile (inputFile);
		} else if (checkOpenTag (inputFile, LineData.LINE_DATA_TAG)) {
			item = LineData.readFromFile (inputFile);
		} else if (checkOpenTag (inputFile, CompoundObjectData.COMPOUND_DATA_TAG)) {
			item = CompoundObjectData.readFromFile (inputFile);
		} else {
			throw new ParsingErrorException ();
		}

		return item;
	}
	*/
  /**
	public static ActionType readActionTypeInTag (BufferedReader inputFile, String tagName)
		throws ParsingErrorException {
		
		ActionType action;
		
		acceptOpenTag (inputFile, tagName);
		action = readActionType (inputFile);
		acceptCloseTag (inputFile, tagName);
	
		return action;
	}
  */

	public static String readStringInTag (BufferedReader inputFile, String tagName) 
		throws ParsingErrorException {

		String s = "";
		acceptOpenTag (inputFile, tagName);
		s = getString (inputFile);
		acceptCloseTag (inputFile, tagName);
		return s;
	}
	
	public static boolean readBooleanInTag (BufferedReader inputFile, String tagName) 
		throws ParsingErrorException {

		boolean b = false;
		acceptOpenTag (inputFile, tagName);
		b = getBoolean (inputFile);
		acceptCloseTag (inputFile, tagName);
		return b;
	}

	public static int readIntInTag (BufferedReader inputFile, String tagName)
		throws ParsingErrorException {

		int i = 0;
		acceptOpenTag (inputFile, tagName);
		i = getInt (inputFile);
		acceptCloseTag (inputFile, tagName);
		return i;
	}

/**		
	public static Point readPointInTag (BufferedReader inputFile, String tagName) 
		throws ParsingErrorException {

		Point p = new Point (0, 0);
		acceptOpenTag (inputFile, tagName);
		p = Point.readFromFile (inputFile);
		acceptCloseTag (inputFile, tagName);
		return p;
	}
*/
	public static void writeOpenTag (Writer output, String tagName) throws IOException {
		output.write (BEGIN_TAG_START);
		output.write (tagName);
		output.write (TAG_END);
	}

	public static void writeCloseTag (Writer output, String tagName) throws IOException {
		output.write (END_TAG_START);
		output.write (tagName);
		output.write (TAG_END);
	}

	public static void writeTaggedBoolean (Writer output, String tagName, boolean data) throws IOException {
		writeOpenTag (output, tagName);
		output.write ("" + data);
		writeCloseTag (output, tagName);
		writeNewLine (output);
	}

	public static void writeTaggedInt (Writer output, String tagName, int data) throws IOException {
		writeOpenTag (output, tagName);
		output.write ("" + data);
		writeCloseTag (output, tagName);
		writeNewLine (output);
	}

	public static void writeTaggedString (Writer output, String tagName, String data) throws IOException {
		writeOpenTag (output, tagName);
		output.write (data);
		writeCloseTag (output, tagName);
		writeNewLine (output);
	}

	public static void writeNewLine (Writer output) throws IOException {
		output.write ('\n');
	}
}

