package patterns;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collection;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Basic tests for the ConcreteExpression class, and the related Expression.of static factory method, but not the
 * ConcreteExpressionTree sub class.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class ConcreteExpressionTest {
    private String str;
    private int num;
    private Expression stringExpression;
    private Expression numberExpression;

    /**
     * Sets up two ConcreteExpression instances for use in tests: one representing a string, another an int.
     */
    @BeforeEach
    void setUp() {
        str = "Hello, World!";
        num = 42;
        stringExpression = Expression.of(str);
        numberExpression = Expression.of(num);
    }

    /**
     * Tests the Expression.of static factory method to ensure the Expression objects created with non-collection values
     * are instances of ConcreteExpression and not ConcreteExpressionTree.
     */
    @Test
    void testExpressionScalarInstantiation() {
        assert stringExpression != null;
        assert stringExpression instanceof ConcreteExpression;
        assert !(stringExpression instanceof ConcreteExpressionTree);
        assert numberExpression != null;
        assert numberExpression instanceof ConcreteExpression;
        assert !(numberExpression instanceof ConcreteExpressionTree);
    }

    /**
     * Tests whether the value of a ConcreteExpression is equal to the object originally passed in the constructor.
     */
    @Test
    void testValue() {
        assert ((ConcreteExpression) stringExpression).value().equals(str);
        assert ((ConcreteExpression) numberExpression).value().equals(num);
    }

    /**
     * Tests whether both ConcreteExpression instances satisfy the requirement of a non-collection ConcreteExpression,
     * viz. that they return an empty list for their sub-expressions.
     */
    @Test
    void testSubExpressions() {
        assert stringExpression.subExpressions() instanceof Collection;
        assert stringExpression.subExpressions().isEmpty();
        assert numberExpression.subExpressions() instanceof Collection;
        assert numberExpression.subExpressions().isEmpty();
    }

    /**
     * Tests whether the equals method for ConcreteExpression returns true when passed another Expression with an
     * equal object, and false otherwise.
     */
    @Test
    void testEquals() {
        assert !stringExpression.equals(numberExpression);
        assert !stringExpression.equals(Expression.of("not " + str));
        assert stringExpression.equals(Expression.of(str));
        assert !numberExpression.equals(Expression.of(1 + num));
        assert numberExpression.equals(Expression.of(num));
    }

    /**
     * Tests whether the hashcode of ConcreteExpression objects with equal values are equal, and false otherwise.
     */
    @Test
    void testHashCode() {
        assert stringExpression.hashCode() != Expression.of("not " + str).hashCode();
        assert stringExpression.hashCode() != numberExpression.hashCode();
        assert stringExpression.hashCode() == Expression.of(str).hashCode();
        assert numberExpression.hashCode() != Expression.of(1 + num).hashCode();
        assert numberExpression.hashCode() == Expression.of(num).hashCode();
    }

    /**
     * Tests whether the toString of a ConcreteExpression is equal to the toString of its value.
     */
    @Test
    void testToString() {
        assert stringExpression.toString().equals(str);
        assert numberExpression.toString().equals(((Integer) num).toString());
    }
}