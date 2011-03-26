package jchrest.tests;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * TestRunner is the parent class for all the individual tests to be 
 * run under junit.  All Test classes are added to the 'SuiteClasses' 
 * annotation.
 *
 * @author Peter C. R. Lane
 */
@RunWith(Suite.class)
@SuiteClasses({TestChrest.class, TestPatterns.class, TestReadModel.class, TestStm.class, TestChessDomain.class})
public class TestRunner {
}
