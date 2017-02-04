package patterns;

import java.util.*;

/** A ConcretePattern is a Pattern which is capable of matching any Expression to which it is compared. Larger Patterns
 * are made up of a combination of PatternVariables and PatternConstants in the internal tree of the Pattern. A
 * ConcretePattern is defined by the Object constant it contains, and thus its String representation is based on that
 * Object.
 * Note: for constructing a tree of Patterns, see the subclass ConcretePatternTree.
 *
 * A ConcretePattern is intended to be immutable, however this can be abused. Avoid using mutable objects as constants,
 * or create a copy of your mutable object and pass it as the constant subsequently deleting any references to it, in
 * order to guarantee immutability.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class ConcretePattern implements Pattern {
    private final Object constant;

    /** Instantiates a new ConcretePattern with the given Object as data, meaning it will only match another
     * ConcretePattern with a constant equal to its own under .equals().
     * Because this class and constructor are package-private, a ConcretePattern cannot be instantiated but through the
     * static-factory method in the Pattern interface; however, all other methods are usable once an instance is had.
     * Note: constant should only be a Collection if invoked by the subclass ConcretePatternTree.
     *
     * TODO: Consider requiring constant be Cloneable, to protect from passing and later changing a mutable value.
     * TODO: Consider implementing a ConcretePatternTree class similar to the ConcreteExpressionTree class.
     *
     * @param constant The constant value which this ConcretePattern represents and solely matches.
     */
    ConcretePattern(final Object constant) {
        this.constant = Objects.requireNonNull(constant, "A Pattern Constant cannot be null.");
    }

    /** A ConcretePattern exists to allow matching with only Expressions which represent the same value as that of the
     * ConcretePattern.
     *
     * TODO: Consider requiring the Object be Cloneable, to allow the returning of a clone and not leaking a reference.
     *
     * @return the internal constant Object.
     */
    public Object constant() {
        return this.constant;
    }

    /** A ConcretePattern will match an Expression whose structure is compatible up to PatternVariable leaves which
     * can match whole branches of an Expression, and whose values at each node are equal to those in the Expression
     * tree, or if the Pattern is a scalar value, it will only match an Expression with an equal scalar value.
     *
     * TODO: Make the .value() member a requirement to fulfill the Expression interface.
     * TODO: Review the comments for this method.
     *
     * @param e The Expression to check for compatibility with this Pattern instance.
     * @return true if the given Expression represents the same value as this.constant.
     */
    @Override
    public boolean matches(final Expression e) {
        if (e == null || !(e instanceof ConcreteExpression)) {
            return false;
        }
        // If they do not represent trees, they must have identical scalar data values.
        return this.constant.equals(((ConcreteExpression) e).value());
    }

    /** Matches the given Expression to this one, and returns a mapping which maps this Pattern to the associated
     * portion of the given Expression. Since this is a ConcretePattern, this will never map to any values which do not
     * match the structure of this Pattern up to any PatternVariables contained in the leaves of the tree, or if it is
     * a simple value, match the value explicitly.
     *
     * TODO: Review the comments for this method.
     *
     * @param e The Expression in which corresponding elements are gathered for the variable terms in this Pattern.
     * @return A map which sends variable components in this Pattern tree to their corresponding Expressions, or an
     *          empty map if this is not a tree.
     */
    @Override
    public Map<Pattern, Expression> match(final Expression e) {
        if (!this.matches(e)) {
            throw new IllegalArgumentException("A Pattern cannot bind to an Expression it does not match with .matches()");
        }
        if (this.constant instanceof Collection) {
            if (e.subExpressions().isEmpty()) {
                // An empty list will not correspond to any Variables in leaf positions, so an empty list is returned.
                return Collections.emptyMap();
            }
            // If the constant is a Collection, the constructor guarantees it will be a Collection<Pattern>.
            Map<Pattern, Expression> accumulateMap = new HashMap<>();
            final Iterator<Expression> expressionIterator = e.subExpressions().iterator();
            final Iterator<Pattern> patternIterator = ((Collection<Pattern>)this.constant).iterator();
            // If the pattern matches the Expression structurally, then there will be an equal number of branches.
            while (expressionIterator.hasNext() && patternIterator.hasNext()) {
                accumulateMap.putAll(patternIterator.next().match(expressionIterator.next()));
            }
            return accumulateMap;
        }
        return Collections.emptyMap();
    }

    /** Creates a new Expression which matches this Pattern given the provided mappings. Since this is a ConcretePattern
     * which can only match an Expression representing the same constant, this will always return an Expression whose
     * value is this.constant.
     *
     * @param bindings The mapping to use for instantiating a new Expression which matches this Pattern.
     * @return an Expression whose value is this.constant
     */
    @Override
    public Expression expressionFrom(final Map<Pattern, Expression> bindings) {
        return Expression.of(this.constant);
    }

    /** Two ConcretePattern instances are equal if and only if they have equal constants under .equals().
     *
     * @param other The object to test for equality with this ConcretePattern instance.
     * @return true if the given object is a ConcretePattern whose constant is equal to this one's, and false otherwise.
     */
    @Override
    public boolean equals(final Object other) {
        if (other == this) {
            return true;
        }
        if (other == null || !(other instanceof ConcretePattern)) {
            return false;
        }
        return this.constant.equals(((ConcretePattern) other).constant);
    }

    /** Because the constant of a ConcretePattern defines the ConcretePattern, the hash of a ConcretePattern instance is
     * the sole consideration in creating the hashcode of this instance.
     *
     * @return a unique integer representing this ConcretePattern instance.
     */
    @Override
    public int hashCode() {
        return 173 * this.constant.hashCode();
    }

    /** Because the constant of a ConcretePattern defines the ConcretePattern, a String representation thereof is a
     * sufficient representation of the ConcretePattern object.
     *
     * @return A String representation of the internal constant object for this ConcretePattern instance.
     */
    @Override
    public String toString() {
        return this.constant.toString();
    }
}
