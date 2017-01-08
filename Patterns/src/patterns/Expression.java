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

    /**
     *
     * @return
     */
    Collection<Expression> subExpressions();
}
