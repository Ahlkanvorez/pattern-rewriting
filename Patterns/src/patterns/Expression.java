package patterns;

import java.lang.reflect.Array;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 *
 * @author robertmitchell
 */
public interface Expression {
    Operator getOperator();
    List<Expression> getSubExpressions();

    String OPERATOR_PATTERN = "[+*-/]+"; // TODO: Support more operators.

    static Expression fromPrefixNotation(final String expr) {
        // TODO: Determine which algorithm is better between this and fromInfixNotation, then implement the lesser in terms of the greater.
        final Deque<String> strings = new ArrayDeque<>();
        /* Convert the string into a stack of strings which are each either an operator, or an operand. */
        Arrays.stream(expr.split("\\s+")).forEach(strings::push);

        final Deque<Expression> expressions = new ArrayDeque<>();
        while (!strings.isEmpty()) {
            final String str = strings.pop();
            if (str.matches(OPERATOR_PATTERN)) {
                final Operator<String> op = Operator.<String>from(str);
                /* Apply this operator to it's needed number of operands. */
                final List<Expression> operands = new ArrayList<>();
                for (int i = 0; i < op.getNumberOfOperands(); i++) {
                    operands.add(expressions.pop());
                }
                /* Add that resulting expression to the list. */
                expressions.push(Expression.from(op, operands.toArray(new Expression[operands.size()])));
            } else {
                expressions.push(new Scalar<>(str));
            }
        }

        return expressions.pop();
    }

    static Expression fromInfixNotation(final String expr) {
        // Pad parentheses so they can be parsed in tokens.
        final Scanner s = new Scanner(expr.replaceAll("\\(", " ( ").replaceAll("\\)", " ) "));

        final Deque<Expression> expressions = new ArrayDeque<>();
        final Deque<Operator> operators = new ArrayDeque<>();
        while (s.hasNext()) {
            final String str = s.next();
            if (str.matches(OPERATOR_PATTERN)) {
                operators.push(Operator.from(str));
            } else if (str.equals(")")) {
                final Operator op = operators.pop();
                final Deque<Expression> operands = new ArrayDeque<>();
                for (int i = 0; i < op.getNumberOfOperands(); ++i) {
                    operands.push(expressions.pop());
                }
                expressions.push(Expression.from(op, operands.toArray(new Expression[operands.size()])));
            } else {
                expressions.push(new Scalar<>(str));
            }
        }

        return expressions.pop();
    }
    
    static Expression from(final Operator op, final Expression ... exprs) {
        // TODO: Consider caching expression instances.
        switch (exprs.length) {
            case 1:
                return new UnaryExpression(op, exprs[0]);
            case 2:
                return new BinaryExpression(op, exprs[0], exprs[1]);
            default:
                throw new UnsupportedOperationException("Too many sub-expressions.");
        }
    }
    
    static boolean isVariable(final Expression expr) {
        return expr != null && expr instanceof Variable;
    }
    
    static boolean isScalar(final Expression expr) {
        return expr != null && expr instanceof Scalar;
    }
    
    static List<Expression> variablesOf(final Expression expr) {
        if (expr == null) {
            return new ArrayList<>();
        } else if (isVariable(expr)) {
            return Arrays.asList(expr);
        }
        final Map<Boolean, List<Expression>> exprGroups = expr.getSubExpressions().stream()
                .collect(Collectors.groupingBy(Expression::isVariable));
                
        final List<Expression> variables = new ArrayList<>();

        if (exprGroups.containsKey(true)) {
            variables.addAll(exprGroups.get(true));
        }
        
        if (exprGroups.containsKey(false)) {
            variables.addAll(exprGroups.get(false).stream()
                .map(Expression::variablesOf)
                .reduce((a, b) -> { a.addAll(b); return a; })
                .get());
        }
        
        assert variables.stream().allMatch(v -> v instanceof Variable);
        
        return variables;
    }
    
    static Expression evaluate(final Expression expr) {
        if (isVariable(expr)) {
            return expr;
        } else if (isScalar(expr)) {
            return expr;
        } else if (variablesOf(expr).isEmpty()) {
            return expr.getOperator()
                    .apply(expr.getSubExpressions().stream()
                        .map(Expression::evaluate)
                        .collect(Collectors.toList()));
        }
        return Expression.from(expr.getOperator(),
                expr.getSubExpressions().stream()
                        .map(Expression::evaluate)
                        .toArray(Expression[]::new));
    }
    
    static void main(final String[] args) {
        final Operator<Double> doubleAddition = new Operator<>("+",
                        a -> a.get(0) + a.get(1), 2);
        final Operator<Double> square = new Operator<>("square",
                        a -> Math.pow(a.get(0), 2), 1);
        final Expression test1 = 
                Expression.from(square,
                    Expression.from(doubleAddition,
                        new Scalar<>(5.0),
                        Expression.from(doubleAddition,
                            new Scalar<>(32.4),
                            new Scalar<>(55.4))));
        
        System.out.println(test1);
        System.out.println(Expression.evaluate(test1));


        Arrays.asList(
                "+ 2 + 3 4",
                "+ + 2 3 4",
                "+ + -- 2 3 4"
        ).forEach(str -> {
            System.out.println(str);
            System.out.println(Expression.fromPrefixNotation(str));
            System.out.println(Expression.evaluate(Expression.fromPrefixNotation(str)));
        });

        System.out.println();

        Arrays.asList(
                "(2 + (3 + 4))",
                "((2 + 3) + 4)",
                "((--(2) + 3) + 4)"
        ).forEach(str -> {
            System.out.println(str);
            System.out.println(Expression.fromInfixNotation(str));
            System.out.println(Expression.evaluate(Expression.fromInfixNotation(str)));
        });
    }
}
