package patterns;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 *
 * @author robertmitchell
 */
public class BinaryExpression implements Expression {
    private final Operator operator;
    private final Expression leftHand;
    private final Expression rightHand;

    public BinaryExpression(final Operator op, final Expression lhs, final Expression rhs) {
        this.operator = Objects.requireNonNull(op);
        this.leftHand = Objects.requireNonNull(lhs);
        this.rightHand = Objects.requireNonNull(rhs);
    }
    
    @Override
    public Operator getOperator() {
        return this.operator;
    }

    @Override
    public List<Expression> getSubExpressions() {
        return Arrays.asList(leftHand, rightHand);
    }
    
    @Override
    public boolean equals(final Object other) {
        if (other == null || !(other instanceof BinaryExpression)) {
            return false;
        }
        final BinaryExpression o = (BinaryExpression) other;
        return this.operator.equals(o.operator)
                && this.leftHand.equals(o.leftHand)
                && this.rightHand.equals(o.rightHand);
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 79 * hash + Objects.hashCode(this.operator);
        hash = 79 * hash + Objects.hashCode(this.leftHand);
        hash = 79 * hash + Objects.hashCode(this.rightHand);
        return hash;
    }
    
    @Override
    public String toString() {
        return String.format("(%s %s %s)", leftHand.toString(), operator.toString(), rightHand.toString());
    }
}
