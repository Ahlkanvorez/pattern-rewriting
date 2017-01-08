package patterns;

import java.util.Map;

/**
 * A Pattern is the basic descriptor of a set of Expressions.
 * In Category Theoretic terms, Expressions are dots, and Rewrite-Rules are arrows; with Patterns partitioning the
 * expressions within the Category of Expressions into sub-categories where each expression is reachable from another by
 * the available Rewrite-Rules, which is to say, given a sub-category defined by a set of Patterns, every Expression in
 * that sub-category will match at least one of the Patterns, and every pair of Patterns is compatible in the sense that
 * there exists a Rewrite-Rule to transform the one into the other.
 * In Grammatical terms, the Expressions are nouns, the Patterns are predicates, and the Rewrite-Rules are verbs acting
 * on the predicates: i.e., the Patterns assert structure in the Expressions, and the Rewrite-Rules (defined by a pair
 * of compatible Patterns) express a state of being where the the first Pattern is written as the second Pattern, which
 * results in the rewritten Expression, differing from the original Expression, and matching the second Pattern.
 *
 * Any class implementing Pattern should be immutable.
 *
 * TODO: Determine whether Pattern should extend Expression.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
public interface Pattern {

    /** This method determines whether the given Expression exhibits the pattern represented by this Pattern instance.
     * A Pattern can be thought of as an abstract Expression tree, in that instead of being an Expression which conveys
     * normal information, a Pattern contains a mixture of variable and scalar elements; viz. elements which will only
     * match that which is identical to them, and elements which specify a structure or characteristic and will match
     * anything which exhibits it. In order for a given Expression to match the Pattern, every sub-expression in the
     * given Expression must match it's corresponding element in the Pattern, as each element specifies, or the match
     * will fail. Of course, an Expression cannot match a Pattern if they exhibit structural differences.
     *
     * @param e The Expression to check for the exhibition of the pattern in this Pattern instance.
     * @return true if the given Expression matches the pattern expressed by this instance, otherwise false.
     */
    boolean matches(Expression e);

    /** Given an Expression which can be matched by this Pattern, this method gathers a mapping of all variable
     * components within the Pattern unto their corresponding elements in the given Expression, and returns that map.
     *
     * @param e The Expression in which corresponding elements are gathered for the variable terms in this Pattern.
     * @return A mapping of all variable terms in this Pattern to the corresponding elements of the given Expression.
     */
    Map<Expression, Expression> match(Expression e);

    /** Given a mapping of bindings, i.e. variable expressions from a Pattern to Expressions from a parent Expression,
     * this method returns a new Expression by mapping the variable portions of the Pattern of this instance to the
     * corresponding Expressions under the given mapping.
     * Note: if there are variable terms in the Pattern of the instance being invoked which are not mapped to an
     * Expression in the given mapping, see the documentation for the implementing class as to how that is handled. A
     * safe assumption is that a NullPointerException will be thrown, however, different behavior is not forbidden.
     *
     * @param bindings The mapping to use for instantiating a new Expression which matches this Pattern.
     * @return A new Expression matching this Pattern using the information from the given mapping.
     */
    Expression expressionFrom(Map<Expression, Expression> bindings);

}
