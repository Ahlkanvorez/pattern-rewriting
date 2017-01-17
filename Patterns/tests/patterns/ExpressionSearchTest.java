package patterns;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Iterator;
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
            List<String> expressions = new ArrayList<>();
            for (Expression subExpr = expr; !subExpr.subExpressions().isEmpty();) {
                final Iterator<Expression> subExprIterator = subExpr.subExpressions().iterator();
                expressions.add(subExprIterator.next().toString());
                subExpr = subExprIterator.next();
            }
            for (int i = expressions.size() - 1; i >= 0; --i) {
                if (expressions.get(i).toString().equals("S")) {
                    sum++;
                } else if (expressions.get(i).toString().equals("N")) {
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
     * The expected result is that an Expression can be reached which evaluates to -12, which in particular is the
     * shortest possible representation of -12 using the two operators of S for the successor function (x -> x + 1), and
     * N for the negative function (x -> -x), viz. [N, [S, [S, [S, [S, [S, [S, [S, [S, [S, [S, [S, [S, 0]]]]]]]]]]]]].
     */
    @Test
    void testPeanoArithmetic() {
        assert search(zero, peanoRules, peanoIsTarget).toString()
                .equals("[N, [S, [S, [S, [S, [S, [S, [S, [S, [S, [S, [S, [S, 0]]]]]]]]]]]]]");
    }
}