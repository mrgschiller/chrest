package jchrest.tests;

import java.io.*;
import org.junit.*;
import static org.junit.Assert.*;

import jchrest.lib.*;

/**
 * Suite of tests to verify save/load of Chrest models.
 * These tests are <em>Unit Tests</em> as they only 
 * affect the performance of the interface to Chrest.
 *
 * @author Peter C. R. Lane
 */
public class TestReadModel {

  @Test public void testSaveNumberPattern () {
    NumberPattern pattern = NumberPattern.create (1);
    StringWriter output = new StringWriter ();
    try {
      pattern.writePattern (output);
      assertTrue (output.toString().equals("<number-pattern>1</number-pattern>\n"));
    } catch (IOException ex) {
      assertTrue (false);
    }
  }

  @Test public void testReadNumberPattern () {
    String pattern = "<number-pattern>1</number-pattern>";

    try {
      NumberPattern pat = NumberPattern.readPattern (new BufferedReader (new StringReader (pattern)));
      assertTrue (pat.equals (NumberPattern.create (1)));
    } catch (ParsingErrorException ex) {
      assertTrue (false);
    }
  }

  @Test public void testSaveStringPattern () {
    StringPattern pattern = StringPattern.create ("name");
    StringWriter output = new StringWriter ();
    try {
      pattern.writePattern (output);
      assertTrue (output.toString().equals("<string-pattern>name</string-pattern>\n"));
    } catch (IOException ex) {
      assertTrue (false);
    }
  }

  @Test public void testReadStringPattern () {
    String pattern = "<string-pattern>name</string-pattern>";

    try {
      StringPattern pat = StringPattern.readPattern (new BufferedReader (new StringReader (pattern)));
      assertTrue (pat.equals (StringPattern.create ("name")));
    } catch (ParsingErrorException ex) {
      assertTrue (false);
    }
  }

  @Test public void testSaveListPattern () {
    ListPattern pattern = Pattern.makeVisualList (new String [] {"a", "b", "c"});
    StringWriter output = new StringWriter ();
    try {
      pattern.writePattern (output);
      assertTrue (output.toString().equals("<list-pattern><items><string-pattern>a</string-pattern>\n<string-pattern>b</string-pattern>\n<string-pattern>c</string-pattern>\n</items><modality>VISUAL</modality>\n<finished>false</finished>\n</list-pattern>"));
    } catch (IOException ex) {
      assertTrue (false);
    }
  }

  @Test public void testReadListPattern () {
    String pattern = "<list-pattern><items><string-pattern>a</string-pattern>\n<string-pattern>b</string-pattern>\n<string-pattern>c</string-pattern>\n</items><modality>VISUAL</modality>\n<finished>false</finished>\n</list-pattern>";

    try {
      ListPattern pat = ListPattern.readPattern (new BufferedReader (new StringReader (pattern)));
      assertTrue (pat.equals (Pattern.makeVisualList (new String [] {"a", "b", "c"})));
    } catch (ParsingErrorException ex) {
      assertTrue (false);
    }
  }
}

