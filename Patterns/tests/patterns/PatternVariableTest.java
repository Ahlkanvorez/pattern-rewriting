package patterns;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class PatternVariableTest {
    private String variableName;
    private Pattern variablePattern;
    private Expression stringExpression;
    private Expression treeExpression;

    @BeforeEach
    void setUp() {
        variableName = "not-x";
        variablePattern = Pattern.of(variableName, true);
        stringExpression = Expression.of("Hello, World!");
        treeExpression = Expression.of(Arrays.asList(stringExpression, stringExpression));
    }

    @Test
    void testStaticFactoryVariableTest() {
        assert variablePattern instanceof PatternVariable;
        try {
            Pattern.variableOf("");
        } catch (Exception e) {
            assert e instanceof IllegalArgumentException;
        }
    }

    @Test
    void testName() {
        assert ((PatternVariable) variablePattern).name().equals(variableName);
    }

    @Test
    void testMatches() {
        assert variablePattern.matches(stringExpression);
        assert variablePattern.matches(treeExpression);
    }

    @Test
    void testMatch() {
        final Map<Pattern, Expression> bindings = new HashMap<>();
        bindings.put(variablePattern, stringExpression);
        assert variablePattern.match(stringExpression).equals(bindings);
        bindings.put(variablePattern, treeExpression);
        assert variablePattern.match(treeExpression).equals(bindings);
    }

    @Test
    void testExpressionFrom() {
        assert variablePattern.expressionFrom(variablePattern.match(stringExpression))
                .equals(stringExpression);
        assert variablePattern.expressionFrom(variablePattern.match(treeExpression))
                .equals(treeExpression);
    }

    @Test
    void testEquals() {
        assert variablePattern.equals(Pattern.variableOf(variableName));
        assert !variablePattern.equals(Pattern.variableOf("not" + variablePattern));
    }

    @Test
    void testHashCode() {
        assert variablePattern.hashCode() == Pattern.variableOf(variableName).hashCode();
        assert variablePattern.hashCode() != Pattern.variableOf("not" + variablePattern).hashCode();
    }

    @Test
    void testToString() {
        assert variablePattern.toString().equals(variableName);
        assert !variablePattern.toString().equals("not" + variablePattern);
    }

}