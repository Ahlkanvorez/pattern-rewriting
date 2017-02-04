package patterns;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

/**
 * TODO: Add tests for a tree pattern with a variable subtree and a constant subtree.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class ConcretePatternTest {
    private String stringInput;
    private Collection<Pattern> collectionInput;
    private Pattern stringPattern;
    private Pattern treePattern;

    private Collection<Expression> equalCollectionExpressionInput;
    private Collection<Expression> differentCollectionExpressionInput;
    private Expression equalStringExpression;
    private Expression differentStringExpression;
    private Expression equalCollectionExpression;
    private Expression differentCollectionExpression;

    /**
     * TODO: Comment
     */
    @BeforeEach
    void setUp() {
        stringInput = "Hello, world!";
        collectionInput = new ArrayList<>();
        stringPattern = Pattern.of(stringInput);
        collectionInput.add(stringPattern);
        treePattern = Pattern.of(collectionInput);

        equalCollectionExpressionInput = new ArrayList<>();
        equalStringExpression = Expression.of(stringInput);
        equalCollectionExpressionInput.add(equalStringExpression);

        differentCollectionExpressionInput = new ArrayList<>();
        differentStringExpression = Expression.of("Farewell, world!");
        differentCollectionExpressionInput.add(differentStringExpression);

        equalCollectionExpression = Expression.of(equalCollectionExpressionInput);
        differentCollectionExpression = Expression.of(differentCollectionExpressionInput);
    }

    /**
     * TODO: Comment
     */
    @Test
    void testStaticFactoryStringPatternTest() {
        assert stringPattern instanceof ConcretePattern;
    }

    /**
     * TODO: Comment
     */
    @Test
    void testStaticFactoryCollectionTest() {
        assert treePattern instanceof ConcretePattern;
        assert ((ConcretePattern) treePattern).constant() instanceof Collection;
    }

    /**
     * TODO: Comment
     */
    @Test
    void testConstant() {
        assert ((ConcretePattern) stringPattern).constant() instanceof String;
        assert ((ConcretePattern) treePattern).constant() instanceof Collection;
    }

    /**
     * TODO: Comment
     */
    @Test
    void testMatches() {
        assert stringPattern.matches(equalStringExpression);
        assert !stringPattern.matches(differentStringExpression);
        assert treePattern.matches(equalCollectionExpression);
        assert !treePattern.matches(differentCollectionExpression);
    }

    /**
     * TODO: Comment
     */
    @Test
    void testMatch() {
        assert stringPattern.match(equalStringExpression).equals(Collections.emptyMap());
        try {
            stringPattern.match(differentStringExpression);
        } catch (Exception e) {
            assert e instanceof IllegalArgumentException;
        }
        assert treePattern.match(equalCollectionExpression).equals(Collections.emptyMap());
        try {
            treePattern.match(differentCollectionExpression);
        } catch (Exception e) {
            assert e instanceof IllegalArgumentException;
        }
    }

    /**
     * TODO: Comment
     */
    @Test
    void testExpressionFrom() {
        assert stringPattern.expressionFrom(Collections.emptyMap()).equals(equalStringExpression);
        assert !stringPattern.expressionFrom(Collections.emptyMap()).equals(differentStringExpression);
        assert treePattern.expressionFrom(Collections.emptyMap()).equals(equalCollectionExpression);
        assert !treePattern.expressionFrom(Collections.emptyMap()).equals(differentCollectionExpression);
    }

    /**
     * TODO: Comment
     */
    @Test
    void testEquals() {
        assert stringPattern.equals(Pattern.of(stringInput));
        assert !stringPattern.equals(treePattern);
        assert treePattern.equals(Pattern.of(collectionInput));
        assert !treePattern.equals(Pattern.of(Arrays.asList(Pattern.of(stringInput + "turtles"))));
    }

    /**
     * TODO: Comment
     */
    @Test
    void testHashCode() {
        assert stringPattern.hashCode() == Pattern.of(stringInput).hashCode();
        assert stringPattern.hashCode() != treePattern.hashCode();
        assert treePattern.hashCode() == Pattern.of(collectionInput).hashCode();
        assert treePattern.hashCode() != Pattern.of(Arrays.asList(Pattern.of(stringInput + "turtles"))).hashCode();
    }

    /**
     * TODO: Comment
     */
    @Test
    void testToString() {
        assert stringPattern.toString().equals(stringInput);
        assert treePattern.toString().equals(equalCollectionExpressionInput.toString());
    }

}