package patterns;

import java.util.Collection;
import java.util.Collections;
import java.util.Objects;

/** An ConcreteExpression is an Expression which should represent a non-collection value. Larger Expressions are made up
 * of a combination of ConcreteExpressions with scalar values and ConcreteExpressionTrees with a list of ConcreteExpressions
 * for values. A ConcreteExpression is defined by the Object value it contains, and thus its String representation is
 * based on that Object.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class ConcreteExpression implements Expression {
    private final Object value;

    /** Instantiates a new ConcreteExpression with the given object.
     * Note: if the value is a Collection, then this constructor should only be invoked from the context
     * of a ConcreteExpressionTree constructor calling super.
     *
     * @param value The value for this ConcreteExpression instance.
     */
    ConcreteExpression(final Object value) {
        this.value = Objects.requireNonNull(value, "An Expression cannot have a null value.");
    }

    /** The value of this ConcreteExpression defines entirely the contents thereof.
     * If the value of this ConcreteExpression is not a scalar, the an immutable copy of the value is returned.
     * Otherwise the value is returned explicitly, and the user is expected not to modify it.
     *
     * @return Either an immutable copy of the Expression tree this instance contains, or the scalar value it contains.
     */
    public Object value() {
        return this.value;
    }

    /** If the value of this ConcreteExpression is not a Collection, then this Expression is a scalar, and consequently
     * has no sub-Expressions. Otherwise, an immutable copy of the sub-Expressions is returned.
     *
     * @return an empty list if this ConcreteExpression is scalar, otherwise a copy of its tree structure.
     */
    @Override
    public Collection<Expression> subExpressions() {
        return Collections.EMPTY_LIST;
    }

    /** Two ConcreteExpression objects are equal if and only if their values are equal.
     *
     * @param other The other object against which to check for equality
     * @return true if the other object is a ConcreteExpression with an equal value, false otherwise.
     */
    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || !(other instanceof ConcreteExpression)) {
            return false;
        }
        return this.value().equals(((ConcreteExpression) other).value());
    }

    /** The value of a ConcreteExpression completely defines it, so its hashcode is based on the hashcode of its value.
     *
     * @return A unique integer representing this ConcreteExpression instance.
     */
    @Override
    public int hashCode() {
        return 11 * value.hashCode();
    }

    /** Because a ConcreteExpression is defined by its value, its own String representation is that of the value.
     *
     * @return a String representing this ConcreteExpression instance.
     */
    @Override
    public String toString() {
        return this.value.toString();
    }
}
