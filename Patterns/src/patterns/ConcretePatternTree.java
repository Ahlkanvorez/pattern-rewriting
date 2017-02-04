package patterns;

import java.util.*;

/** A ConcretePatternTree must have a value of a Collection of ConcretePattern objects, which each can have more lists
 * as objects; thus the list-of-lists-of-etc creates a tree structure of Patterns. The ConcretePattern .equals(...) and
 * .matches(...) methods ensure that equality and match-ability follow the branches down to the leaves of the tree,
 * since two ConcretePattern cannot be equal or match unless their constants are equal, in that case being the subtrees.
 *
 * A ConcretePatternTree is intended to be immutable, however this can be abused. Avoid altering the internal collection
 * via extraneous references, or create a copy of the collection and pass the copy to the constructor, to prevent
 * accidental modifications which would change the Pattern structure.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class ConcretePatternTree extends ConcretePattern {

    /**
     * Instantiates a new ConcretePattern with the given Object as data, meaning it will only match another
     * ConcretePattern with a constant equal to its own under .equals().
     * Because this class and constructor are package-private, a ConcretePattern cannot be instantiated but through the
     * static-factory method in the Pattern interface; however, all other methods are usable once an instance is had.
     * <p>
     * TODO: Consider requiring constant be Cloneable, to protect from passing and later changing a mutable value.
     * TODO: Consider implementing a ConcretePatternTree class similar to the ConcreteExpressionTree class.
     *
     * @param constant The constant value which this ConcretePattern represents and solely matches.
     */
    ConcretePatternTree(final Object constant) {
        super(constant);
        if (!(constant instanceof Collection)) {
            throw new IllegalArgumentException("A Pattern Tree must have a Collection of Patterns for its value.");
        }
        for (final Object elem : (Collection) constant) {
            if (!(elem instanceof Pattern)) {
                throw new IllegalArgumentException("A Pattern Tree cannot have non-Pattern sub-components.");
            }
        }
    }

    /** A ConcretePattern will match an Expression whose structure is compatible up to PatternVariable leaves which
     * can match whole branches of an Expression, and whose values at each node are equal to those in the Expression
     * tree, or if the Pattern is a scalar value, it will only match an Expression with an equal scalar value.
     *
     * @param e The Expression to check for compatibility with this Pattern instance.
     * @return true if the given Expression represents the same value as this.constant.
     */
    @Override
    public boolean matches(Expression e) {
        final Object constant = this.constant();
        ConcreteExpression exp = (ConcreteExpression) e;
        // If the Pattern and Expression do not both contain Collections, i.e. are trees, the match fails.
        if (!(exp.value() instanceof Collection)) {
            return false;
        }
        final Iterator<Expression> expressionIterator = ((Collection<Expression>) exp.value()).iterator();
        final Iterator<Pattern> patternIterator = ((Collection<Pattern>) constant).iterator();
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
            this Pattern matches the given Expression. */
        return true;
    }

    /** Creates a new Expression which matches this Pattern given the provided mappings. Since this is a
     * ConcretePatternTree, it will always return an Expression Tree where each branch is taken from the
     * corresponding branch in this tree.
     *
     * @param bindings The mapping to use for instantiating a new Expression which matches this Pattern.
     * @return an Expression matching
     */
    @Override
    public Expression expressionFrom(Map<Pattern, Expression> bindings) {
        List<Expression> accum = new ArrayList<>();
        // If the constant is a Collection, then the constructor ensures it is a Collection<Pattern>.
        for (final Pattern pat : (Collection<Pattern>) this.constant()) {
            accum.add(pat.expressionFrom(bindings));
        }
        return Expression.of(accum);
    }
}
