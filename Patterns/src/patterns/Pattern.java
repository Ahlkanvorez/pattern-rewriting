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
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
public interface Pattern {

    /**
     *
     * @param e
     * @return
     */
    boolean matches(Expression e);

    /**
     *
     * @param e
     * @return
     */
    Map<Expression, Expression> match(Expression e);

    Expression expressionFrom(Map<Expression, Expression> bindings);

}
