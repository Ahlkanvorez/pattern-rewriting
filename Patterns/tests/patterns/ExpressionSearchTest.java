package patterns;

import algs.datastructures.Stack;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static patterns.ExpressionSearch.search;
import static patterns.RewriteRule.*;

/**
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class ExpressionSearchTest {
    private Expression zero;
    private List<RewriteRule> peanoRules;
    private Function<Expression, Boolean> peanoIsTarget;

    private List<Object> expressionTreeToObjTree(final Expression expr) {
        List<Object> result = new ArrayList<>();

        if (expr instanceof ConcreteExpressionTree) {
            result.addAll(expr.subExpressions().stream().map(subExpr -> {
                if (subExpr instanceof ConcreteExpressionTree) {
                    return expressionTreeToObjTree(subExpr);
                }
                return ((ConcreteExpression) subExpr).value();
            }).collect(Collectors.toList()));
        } else {
            result.add(((ConcreteExpression) expr).value());
        }

        return result;
    }

    private int evalEquation(final Expression expr) {
        return evalEquationFromList(expressionTreeToObjTree(expr));
    }

    private int evalEquationFromList(final List<Object> values) {
        final Stack<String> evalStack = Stack.newInstance();
        switch (values.size()) {
            case 1: // For the sake of this test, must be a number.
                evalStack.push(values.get(0).toString());
                break;
            case 2: // For the sake of this test, unused.
                break;
            case 3: // For the sake of this test, the list will look like [operator, arg, arg]
                evalStack.push(values.get(0).toString());
                if (values.get(1) instanceof Collection) {
                    evalStack.push("" + evalEquationFromList((List<Object>) values.get(1)));
                } else {
                    evalStack.push(values.get(1).toString());
                }
                if (values.get(2) instanceof Collection) {
                    evalStack.push("" + evalEquationFromList((List<Object>) values.get(2)));
                } else {
                    evalStack.push(values.get(2).toString());
                }
                break;
        }
        while (evalStack.size() > 1) {
            final int A = Integer.parseInt(evalStack.pop());
            final int B = Integer.parseInt(evalStack.pop());
            switch (evalStack.pop()) {
                case "+":
                    evalStack.push((A + B) + "");
                    break;
                case "*":
                    evalStack.push((A * B) + "");
                    break;
            }
        }
        return Integer.parseInt(evalStack.pop());
    }

    @BeforeEach
    void setUp() {
        /* Test data for Peano Arithmetic test. */
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