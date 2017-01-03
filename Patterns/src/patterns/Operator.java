package patterns;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 *
 * @author robertmitchell
 */
public class Operator<T> {
    private final String name;
    private final Function<List<T>, T> function;
    private final int numberOfOperands;
    
    public Operator(final String name, final Function<List<T>, T> f, final int numberOfOperands) {
        this.name = Objects.requireNonNull(name);
        this.function = Objects.requireNonNull(f);
        this.numberOfOperands = numberOfOperands;
    }
    
    public Scalar<T> apply(final List<Scalar<T>> operands) {
        return new Scalar<>(function.apply(operands.stream()
                .map(Scalar::getValue)
                .collect(Collectors.toList())));
    }

    public int getNumberOfOperands() {
        return this.numberOfOperands;
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

    private static final Map<Class, Map<String, Operator>> operators = new HashMap<>();

    static {
        operators.put(Double.class, new HashMap<>());
        operators.put(Integer.class, new HashMap<>());
        operators.put(String.class, new HashMap<>());

        operators.get(String.class).put("+", new Operator<String>("+",
                operands -> "" + (Integer.parseInt(operands.get(0)) + Integer.parseInt(operands.get(1))), 2));
        operators.get(String.class).put("-", new Operator<String>("-",
                operands -> "" + (Integer.parseInt(operands.get(0)) + Integer.parseInt(operands.get(1))), 2));
        operators.get(String.class).put("--", new Operator<String>("--",
                operands -> "" + (-Integer.parseInt(operands.get(0))), 1));

    }

    public static Operator<String> from(final String s) {
        // TODO: Make it lookup the name in a map, then return the appropriate operator for the requested type and name.
        return operators.get(String.class).get(s);
    }
}
