package patterns;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Objects;

/** An ConcreteExpressionTree is an Expression which should represent a collection value. Larger Expressions are made up
 * of ConcreteExpressionTrees with Expressions in their underlying Collections. A ConcreteExpressionTree is defined by
 * the Collection value it contains, and thus its String representation is based on that Object.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class ConcreteExpressionTree extends ConcreteExpression {

    /** Instantiates a new ConcreteExpressionTree with the given list of Expressions for the value.
     * Note that the given value must implement Collection, and must contain only Expression instances.
     *
     * TODO: Consider supporting a Map value.
     *
     * @param value The collection of Expressions for this ConcreteExpressionTree instance.
     */
    ConcreteExpressionTree (final Object value) {
        super(Objects.requireNonNull(value, "An Expression tree cannot have a null value."));
        if (!(value instanceof Collection)) {
            throw new IllegalArgumentException("An Expression tree must have a collection of Expression for a value.");
        }
        for (final Object obj : (Collection) value) {
            if (!(obj instanceof Expression)) {
                throw new IllegalArgumentException("An Expression tree cannot contain non-Expression objects. " + obj.getClass());
            }
        }
    }

    /** Note: because the value of this Expression is simply the collection of sub expressions, this method is
     * equivalent to value().
     *
     * @return a collection containing all the subExpressions in this Expression Tree.
     */
    @Override
    public Collection<Expression> subExpressions () {
        // Create a new collection to protect the interior collection, but don't return an unmodifiable collection
        // because the unmodifiable collections don't use the internal collections .equals() method, and thus break
        // comparisons for equality.
        return new ArrayList<>((Collection<Expression>) super.value());
    }

    /** Note: because the value of this Expression is simply the collection of sub expressions, this method is
     * equivalent to subExpressions().
     *
     * @return an unmodifiable version of the underlying Collection for this Expression Tree.
     */
    @Override
    public Object value () {
        return this.subExpressions();
    }

    @Override
    public String toString () {
        final StringBuilder result = new StringBuilder("(");
        for (final Expression s : this.subExpressions()) {
            result.append(s.toString()).append(" ");
        }
        return result.toString().trim() + ")";
    }
}
