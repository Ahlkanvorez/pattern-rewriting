package patterns;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 *
 * @author robertmitchell
 * @param <T>
 */
public class Operator<T> {
    private final String name;
    private final Function<List<T>, T> function;
    
    public Operator(final String name, Function<List<T>, T> f) {
        this.name = Objects.requireNonNull(name);
        this.function = Objects.requireNonNull(f);
    }
    
    public Scalar<T> apply(final List<Scalar<T>> operands) {
        return new Scalar<>(function.apply(operands.stream()
                .map(Scalar::getValue)
                .collect(Collectors.toList())));
    }
    
    @Override
    public boolean equals(final Object other) {
        if (other == null || !(other instanceof Operator)) {
            return false;
        }
        final Operator o = (Operator) other;
        return this.name.equals(o.name)
                && this.function.equals(o.function);
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 23 * hash + Objects.hashCode(this.name);
        hash = 23 * hash + Objects.hashCode(this.function);
        return hash;
    }
    
    @Override
    public String toString() {
        return this.name;
    }
}
