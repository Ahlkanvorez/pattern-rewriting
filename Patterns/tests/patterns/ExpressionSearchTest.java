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

    private Expression equation;
    private List<RewriteRule> groupRules;

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
        Stack<String> evalStack = Stack.newInstance();
        System.out.println();
        System.out.println("Values: " + values);
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
        System.out.println(evalStack);
        while (evalStack.size() > 1) {
            int A = Integer.parseInt(evalStack.pop());
            int B = Integer.parseInt(evalStack.pop());
            String op = evalStack.pop();
            if (op.equals("+")) {
                System.out.printf("%d + %d = %d%n", A, B, A + B);
                evalStack.push((A + B) + "");
            } else if (op.equals("*")) {
                System.out.printf("%d * %d = %d%n", A, B, A * B);
                evalStack.push((A * B) + "");
            }
        }
        return Integer.parseInt(evalStack.pop());
    }

    private Function<Expression, Boolean> equationIsTarget(final int target) {
        return expr -> ((ConcreteExpression) expr).value().toString().equals(target + "");
    };

    /**
     * TODO: Comment
     */
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

        /* Test data for algebraic manipulation test. */
        groupRules = new ArrayList<>();
        /* The test equation is + 3 * 8 0, which is prefix notation for 3 + (8 * 0) */
        equation = Expression.of(Arrays.asList(
                Expression.of("+"),
                Expression.of(3),
                Expression.of(Arrays.asList(
                        Expression.of("*"),
                        Expression.of("8"),
                        Expression.of("0")
                ))
        ));

        /* Add rules for commutativity of Multiplication and Addition
            These patterns look as follows:
             - (* A B) -> (* B A)
             - (+ A B) -> (+ B A) */
        groupRules.addAll(Stream.of("*", "+").
                map(op -> {
                    final Pattern aOpB = Pattern.of(Arrays.asList(
                            Pattern.of(op),
                            Pattern.variableOf("A"),
                            Pattern.variableOf("B")
                    ));
                    final Pattern bOpA = Pattern.of(Arrays.asList(
                            Pattern.of(op),
                            Pattern.of("B"),
                            Pattern.of("A")
                    ));
                    return RewriteRule.ruleFrom(aOpB, bOpA);
                }).
                collect(Collectors.toList()));
        /* Add multiplying by zero rule. Note: since commutative rules exist for multiplication, only one statement of
            this rule is needed. Also, for the sake testing, invoking multiplicative commutativity is required to pass.
            This pattern looks as follows: (* 0 x) -> 0 */
        groupRules.add(RewriteRule.ruleFrom(
                Pattern.of(Arrays.asList(
                        Pattern.of("*"),
                        Pattern.of(0),
                        Pattern.variableOf("x")
                )),
                Pattern.of(0)));
        /* Add adding by zero rule. Note: since commutativity rules exist for addition, only one statement of this rule
            is needed. Also, for the sake of testing, invoking additive commutativity is required to pass.
            This pattern looks as follows: (+ 0 x) -> x */
        groupRules.add(RewriteRule.ruleFrom(
                Pattern.of(Arrays.asList(
                        Pattern.of("+"),
                        Pattern.of(0),
                        Pattern.variableOf("x")
                )),
                Pattern.variableOf("x")
        ));
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

    /**
     * TODO: Fix this test, and comment it.
     */
    @Test
    void testAlgebraicManipulation() {
        System.out.println(equation + " " + groupRules);
        assert search(equation, groupRules, equationIsTarget(0)).toString()
                .equals("0");
    }
}