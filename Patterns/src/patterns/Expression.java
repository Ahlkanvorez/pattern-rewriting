package patterns;

import java.lang.reflect.Array;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * An Expression is the basic object which Patterns match.
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
 * An Expression can be made up of sub-expressions, or it can be an indivisible unit.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
public interface Expression {

    /** Every Expression is either an indivisible unit, or is composed of some number of subexpressions. The
     * subexpressions are used for matching Patterns, and rewriting to create new Expressions using RewriteRules. An
     * Expression can be thought of as a tree, where each subtree is a subexpression, and each node in the tree contains
     * some sort of information defining the contents of the Expression, with the number and location of child-branches
     * defining the structure of the Expression. A Pattern matches an Expression if it has a compatible structure, which
     * can either be identical, or can match by way of variable elements matching whole subtrees within the Expression,
     * and if every non-variable element is identical to the corresponding non-variable element in the Expression.
     *
     * @return a collection of the subexpressions making up this Expression, or null if this Expression is an unit.
     */
    Collection<Expression> subExpressions();
}
