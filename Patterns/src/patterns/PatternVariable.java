package patterns;

import java.util.HashMap;
import java.util.Map;

/**
 * A Pattern Variable is Immutable.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
public final class PatternVariable implements Pattern {
    private final String name;

    /** Instantiates a new PatternVariable with the given name, which is used in representing the Pattern in text.
     * A PatternVariable is intended to match any valid Expression, and to be used in constructing larger Patterns which
     * are capable of matching unspecified Expressions at some part of their internal Pattern-tree.
     *
     * @param name The name to use in representing this PatternVariable
     */
    public PatternVariable(final String name) {
        if (name == null || name.equals("")) {
            throw new IllegalArgumentException("A Pattern Variable can have neither a null name nor an empty name.");
        }
        this.name = name;
    }

    /** The name of a PatternVariable is the only string representation it has.
     * It is used when displaying the PatternVariable either alone or within a parent Pattern as a String.
     *
     * @return The String label for this PatternVariable.
     */
    public String name() {
        return this.name;
    }

    /** A PatternVariable can match any Expression, so this method returns true for any Expression given.
     *
     * TODO: Determine whether a PatternVariable should be able to match null.
     *
     * @param e The Expression to check for the exhibition of the pattern in this Pattern instance.
     * @return true, because a PatternVariable can match any Expression.
     */
    @Override
    public boolean matches(Expression e) {
        return true;
    }

    /** Creates a mapping from this PatternVariable to the given Expression, since this Pattern can match any
     * Expression.
     *
     * TODO: Determine whether a PatternVariable should be able to match null.
     *
     * @param e The Expression in which corresponding elements are gathered for the variable terms in this Pattern.
     * @return A mapping from this PatternVariable object to the given Expression object.
     */
    @Override
    public Map<Pattern, Expression> match(Expression e) {
        final Map<Pattern, Expression> bindings = new HashMap<>();
        bindings.put(this, e);
        return bindings;
    }

    /** Given a mapping of patterns to expressions, this returns a new Expression by replacing this PatternVariable with
     * it's associated Expression under the given mapping.
     *
     * @param bindings The mapping to use for instantiating a new Expression which matches this Pattern.
     * @return The Expression corresponding to this PatternVariable object under the given mapping.
     */
    @Override
    public Expression expressionFrom(Map<Pattern, Expression> bindings) {
        return bindings.get(this);
    }

    /** Two PatternVariable instances are equal if and only if their underlying names are equal.
     * Because the name of a PatternVariable is the only identifier available in a String format for the instance, two
     * PatternVariable instances with identical names must be considered equal to avoid ambiguous String representations
     * which would cause headaches in human users.
     *
     * @param other The object to check for equality with this PatternVariable.
     * @return true if the other object is a PatternVariable with an identical name to that of this one, otherwise false
     */
    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || !(other instanceof PatternVariable)) {
            return false;
        }
        return this.name.equals(((PatternVariable)other).name());
    }

    /** The hashcode of a PatternVariable object is based solely on the hashcode of its name.
     *
     * @return A unique integer representing this PatternVariable object for use in hashing.
     */
    @Override
    public int hashCode() {
        return 17 * this.name.hashCode();
    }

    /** A PatternVariable has one String representation, its name.
     *
     * TODO: Consider additional syntax for indicating this is a PatternVariable and not a String PatternConstant,
     * TODO: e.g. "(variable " + this.name() + ")" Of course, any designation could be mimicked in a PatternConstant.
     *
     * @return The name of this PatternVariable as a String, which is its only String representation.
     */
    @Override
    public String toString() {
        return this.name();
    }
}
