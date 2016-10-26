package patterns;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 *
 * @author robertmitchell
 * @param <T> The type of the scalar
 */
public class Scalar<T> implements Expression {
    private final T value;
    
    public Scalar(final T value) {
        this.value = value;
    }

    @Override
    public Operator getOperator() {
        return null; // Scalars do not have operators.
    }

    @Override
    public List<Expression> getSubExpressions() {
        return new ArrayList<>(); // Scalars have no sub-expressions.
    }
    
    public T getValue() {
        return this.value;
    }
    
    @Override
    public boolean equals(Object other) {
        if (other == null || !(other instanceof Scalar)) {
            return false;
        }
        return this.value.equals(((Scalar) other).value);
    }
 
    @Override
    public int hashCode() {
        int hash = 7;
        hash = 97 * hash + Objects.hashCode(this.value);
        return hash;
    }
   
    @Override
    public String toString() {
        return value.toString();
    }
}