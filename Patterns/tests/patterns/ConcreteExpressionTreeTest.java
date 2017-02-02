package patterns;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Basic tests for the ConcreteExpressionTree class, and the related Expression.of static factory method, but not the
 * ConcreteExpression parent class.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class ConcreteExpressionTreeTest {
    private Expression nums;

    /**
     * Sets up an Expression tree which looks like [1, 2, 3] for use in all the tests.
     */
    @BeforeEach
    void setUp() {
        nums = Expression.of(Arrays.asList(
                Expression.of(1),
                Expression.of(2),
                Expression.of(3)));
    }

    /**
     * Checks that the Expression.of static factory method correctly returns a ConcreteExpressionTree instance when
     * given an object implementing Collection, and a ConcreteExpression for other objects.
     */
    @Test
    void testExpressionTreeInstantiation() {
        assert Expression.of(Arrays.asList(Expression.of("a"))) instanceof ConcreteExpressionTree;
        assert !(Expression.of(1) instanceof ConcreteExpressionTree);
    }

    /**
     * Tests that the sub-expressions in the nums expression have their original order preserved in the return value of
     * .subExpressions(), and that the correct number of sub expressions is returned.
     */
    @Test
    void testSubExpressions() {
        assert nums.subExpressions() instanceof Collection;
        final List<Expression> numbers = new ArrayList<>(nums.subExpressions());
        assert numbers.get(0).equals(Expression.of(1));
        assert numbers.get(1).equals(Expression.of(2));
        assert numbers.get(2).equals(Expression.of(3));
        assert numbers.size() == 3;
    }

    /**
     * Checks that the value() method of the ConcreteExpressionTree class returns an equal representation of the sub
     * expressions to that of the .subExpression() method, since the value of a ConcreteExpressionTree is simply the
     * collection of sub expressions.
     */
    @Test
    void testValue() {
        /* Because UnmodifiableCollection uses the Object.equals method, and not the List.equals method, we have to wrap
         * the Collections in a different Collection to compare values; in this case an ArrayList is used.
         */
        assert new ArrayList<>((Collection<Expression>)((ConcreteExpressionTree) nums).value())
                .equals(new ArrayList<>(nums.subExpressions()));
    }
}