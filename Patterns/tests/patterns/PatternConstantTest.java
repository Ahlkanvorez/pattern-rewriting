package patterns;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Created by robertmitchell on 1/16/17.
 */
class PatternConstantTest {
    private String stringInput;
    private String variableName;
    private Collection<Pattern> collectionInput;
    private Pattern stringPattern;
    private Pattern treePattern;
    private Pattern variablePattern;

    private Collection<Expression> equalCollectionExpressionInput;
    private Collection<Expression> differentCollectionExpressionInput;
    private Expression equalStringExpression;
    private Expression differentStringExpression;
    private Expression equalCollectionExpression;
    private Expression differentCollectionExpression;


    @BeforeEach
    void setUp() {
        variableName = "not-x";
        variablePattern = Pattern.of(variableName, true);

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

    @Test
    void testStaticFactoryVariableTest() {
        assert variablePattern instanceof PatternVariable;
    }

    @Test
    void testStaticFactoryStringPatternTest() {
        assert stringPattern instanceof PatternConstant;
    }

    @Test
    void testStaticFactoryCollectionTest() {
        assert treePattern instanceof PatternConstant;
        assert ((PatternConstant) treePattern).constant() instanceof Collection;
    }

    @Test
    void testConstant() {
        assert ((PatternConstant) stringPattern).constant() instanceof String;
        assert ((PatternConstant) treePattern).constant() instanceof Collection;
    }

    @Test
    void testMatches() {
        assert stringPattern.matches(equalStringExpression);
        assert !stringPattern.matches(differentStringExpression);
        assert treePattern.matches(equalCollectionExpression);
        assert !treePattern.matches(differentCollectionExpression);
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