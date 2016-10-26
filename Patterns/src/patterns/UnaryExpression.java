package patterns;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * NOTE: Immutable class.
 *
 * @author robertmitchell
 */
public class UnaryExpression implements Expression {
    private final Operator operator;
    private final Expression subExpression;

    public UnaryExpression(final Operator op, final Expression subExpr) {
        this.operator = Objects.requireNonNull(op);
        this.subExpression = Objects.requireNonNull(subExpr);
    }
    
    @Override
    public Operator getOperator() {
        return this.operator;
    }

    @Override
    public List<Expression> getSubExpressions() {
        return Arrays.asList(subExpression);
    }
    
    @Override
    public boolean equals(final Object other) {
        if (other == null || !(other instanceof UnaryExpression)) {
            return false;
        }
        final UnaryExpression o = (UnaryExpression) other;
        return this.operator.equals(o.operator)
                && this.subExpression.equals(o.subExpression);
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 43 * hash + Objects.hashCode(this.operator);
        hash = 43 * hash + Objects.hashCode(this.subExpression);
        return hash;
    }
    
    @Override
    public String toString() {
        return String.format("%s(%s)", operator.toString(), subExpression.toString());
    }
}
