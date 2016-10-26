package patterns;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 *
 * @author robertmitchell
 */
public interface Expression {
    public Operator getOperator();
    public List<Expression> getSubExpressions();
    
    public static Expression from(final String s) {
        // TODO: Implement parsing expression trees from a String.
        return null;
    }
    
    public static Expression from(final Operator op, final Expression ... exprs) {
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
    
    public static boolean isVariable(final Expression expr) {
        return expr != null && expr instanceof Variable;
    }
    
    public static boolean isScalar(final Expression expr) {
        return expr != null && expr instanceof Scalar;
    }
    
    public static List<Expression> variablesOf(final Expression expr) {
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
    
    public static Expression evaluate(final Expression expr) {
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
    
    public static void main(final String[] args) {
        final Operator<Double> doubleAddition = new Operator<>("+",
                        a -> a.get(0) + a.get(1));
        final Operator<Double> square = new Operator<>("square",
                        a -> Math.pow(a.get(0), 2));
        final Expression test1 = 
                Expression.from(square,
                    Expression.from(doubleAddition,
                        new Scalar<>(5.0),
                        Expression.from(doubleAddition,
                            new Scalar<>(32.4),
                            new Scalar<>(55.4))));
        
        System.out.println(test1);
        System.out.println(Expression.evaluate(test1));
    }
}
