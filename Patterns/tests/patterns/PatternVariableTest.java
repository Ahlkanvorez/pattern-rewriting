package patterns;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Created by robertmitchell on 1/16/17.
 */
class PatternVariableTest {
    private String variableName;
    private Pattern variablePattern;

    @BeforeAll
    void setUp() {
        variableName = "not-x";
        variablePattern = Pattern.of(variableName, true);
    }

    @Test
    void testStaticFactoryVariableTest() {
        assert variablePattern instanceof PatternVariable;
    }

    @Test
    void testName() {

    }

    @Test
    void testMatches() {

    }

    @Test
    void testMatch() {

    }

    @Test
    void testExpressionFrom() {

    }

    @Test
    void testEquals() {

    }

    @Test
    void testHashCode() {

    }

    @Test
    void testToString() {

    }

}