package patterns;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static patterns.ExpressionSearch.search;
import static patterns.RewriteRule.*;

/**
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class ExpressionSearchTest {
    Expression zero;
    List<RewriteRule> peanoRules;
    Function<Expression, Boolean> peanoIsTarget;

    @BeforeEach
    void setUp() {
        zero = Expression.of(0);

        peanoRules = new ArrayList<>();
        List<Pattern> successorSubPattern = new ArrayList<>();
        successorSubPattern.add(Pattern.of("S"));
        successorSubPattern.add(Pattern.variableOf("x"));
        peanoRules.add(ruleFrom(Pattern.variableOf("x"), Pattern.of(successorSubPattern)));

        List<Pattern> negationSubPattern = new ArrayList<>();
        negationSubPattern.add(Pattern.of("N"));
        negationSubPattern.add(Pattern.variableOf("x"));
        peanoRules.add(ruleFrom(Pattern.variableOf("x"), Pattern.of(negationSubPattern)));

        final int peanoTargetInt = -12;
        peanoIsTarget = expr -> {
            if (expr.subExpressions().isEmpty()) {
                return false;
            }
            int sum = 0;
            for (Expression e : expr.subExpressions()) {
                if (e.toString().equals("S")) {
                    sum++;
                } else if (e.toString().equals("N")) {
                    sum = -sum;
                }
            }
            return sum == peanoTargetInt;
        };
    }

    /**
     * The expected result is that a search with all null arguments will throw a NullPointerException.
     */
    @Test
    void testSearchAllArgumentsNull() {
        try {
            search(null, null, null);
        } catch (Exception e) {
            assert e instanceof NullPointerException;
        }
    }

    /**
     * The expected result is that an Expression can be reached which evaluates to -12.
     */
    @Test
    void testPeanoArithmetic() {
        assert search(zero, peanoRules, peanoIsTarget) != null;
    }
}