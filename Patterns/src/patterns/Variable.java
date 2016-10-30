package patterns;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 *
 * @author robertmitchell
 */
public class Variable implements Expression {
    private final String name;
    
    public Variable(final String name) {
        // TODO: Cache instances by name.
        this.name = Objects.requireNonNull(name);
    }

    @Override
    public Operator getOperator() {
        return null;
    }

    @Override
    public List<Expression> getSubExpressions() {
        return new ArrayList<>();
    }
    
    @Override
    public boolean equals(final Object other) {
        if (other == null || !(other instanceof Variable)) {
            return false;
        }
        return this.name.equals(((Variable) other).toString());
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 97 * hash + Objects.hashCode(this.name);
        return hash;
    }
    
    @Override
    public String toString() {
        return this.name;
    }
}
