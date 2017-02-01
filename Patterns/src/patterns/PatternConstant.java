package patterns;

import java.util.*;

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
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
final class PatternConstant implements Pattern {
    private final Object constant;

    /** Instantiates a new PatternConstant with the given Object as data, meaning it will only match another
     * PatternConstant with a constant equal to its own under .equals().
     * Because this class and constructor are package-private, a PatternConstant cannot be instantiated but through the
     * static-factory method in the Pattern interface; however, all other methods are usable once an instance is had.
     *
     * TODO: Consider requiring constant be Cloneable, to protect from passing and later changing a mutable value.
     *
     * @param constant The constant value which this PatternConstant represents and solely matches.
     */
    PatternConstant(final Object constant) {
        if (constant instanceof Collection) {
            for (final Object elem : (Collection) constant) {
                if (!(elem instanceof Pattern)) {
                    throw new IllegalArgumentException("A Pattern cannot have non-Pattern sub-components.");
                }
            }
        }
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

    /** A PatternConstant will match an Expression whose structure is compatible up to PatternVariable leaves which
     * can match whole branches of an Expression, and whose values at each node are equal to those in the Expression
     * tree, or if the Pattern is a scalar value, it will only match an Expression with an equal scalar value.
     *
     * TODO: Make the .value() member a requirement to fulfill the Expression interface.
     * TODO: Review the comments for this method.
     *
     * @param e The Expression to check for the exhibition of the pattern in this Pattern instance.
     * @return true if the given Expression represents the same value as this.constant.
     */
    @Override
    public boolean matches(final Expression e) {
        if (e == null || !(e instanceof ConcreteExpression)) {
            return false;
        }
        ConcreteExpression exp = (ConcreteExpression) e;
        if (this.constant instanceof Collection) {
            // If the Pattern and Expression do not both contain Collections, i.e. are trees, the match fails.
            if (!(exp.value() instanceof Collection)) {
                return false;
            }
            final Iterator<Expression> expressionIterator = ((Collection<Expression>) exp.value()).iterator();
            final Iterator<Pattern> patternIterator = ((Collection<Pattern>) this.constant).iterator();
            while (expressionIterator.hasNext() && patternIterator.hasNext()) {
                // If any of the branches do not match, the whole match fails.
                if (!patternIterator.next().matches(expressionIterator.next())) {
                    return false;
                }
            }
            // If the Pattern and the Expression have an unequal number of branches at this node, the match fails.
            if (patternIterator.hasNext() || expressionIterator.hasNext()) {
                return false;
            }
            /* Otherwise, they have an equal number of branches at this node, and each branch is equal to the other, so
                this Pattern matches the given Expression.
             */
            return true;
        }
        // If they do not represent trees, they must have identical scalar data values.
        return this.constant.equals(exp.value());
    }

    /** Matches the given Expression to this one, and returns a mapping which maps this Pattern to the associated
     * portion of the given Expression. Since this is a PatternConstant, this will never map to any values which do not
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

    /** Creates a new Expression which matches this Pattern given the provided mappings. Since this is a PatternConstant
     * which can only match an Expression representing the same constant, this will always return an Expression whose
     * value is this.constant.
     *
     * @param bindings The mapping to use for instantiating a new Expression which matches this Pattern.
     * @return an Expression whose value is this.constant
     */
    @Override
    public Expression expressionFrom(final Map<Pattern, Expression> bindings) {
        if (this.constant instanceof Collection) {
            List<Expression> accum = new ArrayList<>();
            // If the constant is a Collection, then the constructor ensures it is a Collection<Pattern>.
            for (final Pattern pat : (Collection<Pattern>) this.constant) {
                accum.add(pat.expressionFrom(bindings));
            }
            return Expression.of(accum);
        }
        return Expression.of(this.constant);
    }

    /** Two PatternConstant instances are equal if and only if they have equal constants under .equals().
     *
     * @param other The object to test for equality with this PatternConstant instance.
     * @return true if the given object is a PatternConstant whose constant is equal to this one's, and false otherwise.
     */
    @Override
    public boolean equals(final Object other) {
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
