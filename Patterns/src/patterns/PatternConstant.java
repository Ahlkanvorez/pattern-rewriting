package patterns;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/** A PatternConstant is a Pattern which is capable of matching any Expression to which it is compared. Larger Patterns
 * are made up of a combination of PatternVariables and PatternConstants in the internal tree of the Pattern. A
 * PatternConstant is defined by the Object constant it contains, and thus its String representation is based on that
 * Object.
 * Note: for constructing a tree of Patterns, the constant Object of a PatternConstant can be made a list of
 * PatternConstant objects, which each can have more lists as objects; thus the list-of-lists-of-etc creates a tree
 * structure of Patterns, and the PatternConstant .equals(...) and .matches(...) methods ensure that equality and
 * matchability follow the branches down to the leaves of the tree, since two PatternConstant cannot be equal or match
 * unless their constants are equal, in that case being the subtrees.
 *
 * A PatternConstant is intended to be immutable, however this can be abused. Avoid using mutable objects as constants,
 * or create a copy of your mutable object and pass it as the constant subsequently deleting any references to it, in
 * order to guarantee immutability.
 *
 * TODO: Consider making this class package private and adding a static factory method to Pattern for its instantiation.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
public final class PatternConstant implements Pattern {
    private final Object constant;

    /** Instantiates a new PatternConstant with the given Object as data, meaning it will only match another
     * PatternConstant with a constant equal to its own under .equals().
     *
     * TODO: Consider requiring constant be Cloneable, to protect from passing and later changing a mutable value.
     *
     * @param constant The constant value which this PatternConstant represents and solely matches.
     */
    public PatternConstant(final Object constant) {
        this.constant = Objects.requireNonNull(constant, "A Pattern Constant cannot be null.");
    }

    /** A PatternConstant exists to allow matching with only Expressions which represent the same value as that of the
     * PatternConstant.
     *
     * TODO: Consider requiring the Object be Cloneable, to allow the returning of a clone and not leaking a reference.
     *
     * @return the internal constant Object.
     */
    public Object constant() {
        return this.constant;
    }

    /** A PatternConstant will only match with an Expression whose internal value is equal to this.constant.
     *
     * @param e The Expression to check for the exhibition of the pattern in this Pattern instance.
     * @return true if the given Expression represents the same value as this.constant.
     */
    @Override
    public boolean matches(Expression e) {
        if (e == null || !(e instanceof ConcreteExpression)) {
            return false;
        }
        return this.constant.equals(((ConcreteExpression) e).value());
    }

    /** Matches the given Expression to this one, and returns a mapping which maps this Pattern to the associated
     * portion of the given Expression. Since this is a PatternConstant, this will never map any values in a mapping,
     * because there are no variable portions to map; consequently, an empty map is always returned, and during the
     * rewrite process when this empty map is passed to expressionsFrom(...), an appropriate Expression is returned
     * with the value represented by this PatternConstant.
     *
     * @param e The Expression in which corresponding elements are gathered for the variable terms in this Pattern.
     * @return An empty mapping, because a PatternConstant has no variable portions to bind to the given Expression.
     */
    @Override
    public Map<Pattern, Expression> match(Expression e) {
        return Collections.EMPTY_MAP;
    }

    /** Creates a new Expression which matches this Pattern given the provided mappings. Since this is a PatternConstant
     * which can only match an Expression representing the same constant, this will always return an Expression whose
     * value is this.constant.
     *
     * @param bindings The mapping to use for instantiating a new Expression which matches this Pattern.
     * @return an Expression whose value is this.constant
     */
    @Override
    public Expression expressionFrom(Map<Pattern, Expression> bindings) {
        return new ConcreteExpression(this.constant);
    }

    /** Two PatternConstant instances are equal if and only if they have equal constants under .equals().
     *
     * @param other The object to test for equality with this PatternConstant instance.
     * @return true if the given object is a PatternConstant whose constant is equal to this one's, and false otherwise.
     */
    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if (other == null || !(other instanceof PatternConstant)) {
            return false;
        }
        return this.constant.equals(((PatternConstant) other).constant);
    }

    /** Because the constant of a PatternConstant defines the PatternConstant, the hash of a PatternConstant instance is
     * the sole consideration in creating the hashcode of this instance.
     *
     * @return a unique integer representing this PatternConstant instance.
     */
    @Override
    public int hashCode() {
        return 173 * this.constant.hashCode();
    }

    /** Because the constant of a PatternConstant defines the PatternConstant, a String representation thereof is a
     * sufficient representation of the PatternConstant object.
     *
     * @return A String representation of the internal constant object for this PatternConstant instance.
     */
    @Override
    public String toString() {
        return this.constant.toString();
    }
}
