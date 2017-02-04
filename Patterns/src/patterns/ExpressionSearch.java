package patterns;

import algs.search.BreadthFirstSearch;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

/** This class provides searching functionality on Expression graphs generated from a list of RewriteRule instances.
 * This can be useful when determining if a particular Expression can be derived from a source Expression given a set
 * of rules, or if some composition of RewriteRules can make a source Expression match some other Pattern which is not
 * represented in the Rules.
 *
 * A few example applications of this:
 * - Searching from a Mathematical equation with RewriteRules which perform operations to rewrite the equation, in a
 * search for either a form representing a solution, or a form equal to some desired target.
 * - Searching from an axiom using RewriteRules representing valid theorems in a search for some yet unproven theorem,
 * a successful search being a proof of that theorem from the used axiom and theorems.
 * - Searching from an Expression representing a Chess board with RewriteRules representing valid moves for a board
 * which matches a Pattern representing a Checkmate, to see if a game can be won from a particular setup.
 * - Searching from a natural language String using RewriteRules that exchange synonymous phrases and words with one
 * another to see if two sentences might mean the same thing (or at least, can be equivocated).
 * - Searching from a string representing a program with RewriteRules that perform potential optimizations and with a
 * predicate that will evaluate and benchmark the given program Expression to see if a faster version can be found
 * automatically.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
public class ExpressionSearch {

    /** Given an Expression from which to start, a list of RewriteRules to use, and a function to determine whether a
     * particular Expression is the desired target Expression, this method will perform a Breadth First Search using
     * the provided data along the Expression graph constructed with the given RewriteRules, returning the first
     * encountered Expression which satisfies the provided predicate.
     * Note: If the RewriteRules generate a graph with an infinite number of distinct Expressions, this method will
     * continue to search until it finds a satisfactory Expression, which may not be found before Java runs out of
     * memory. Be wary of what RewriteRules you provide; the search will automatically not consider duplicate
     * Expressions, but unreasonable requests will respond with a reasonable OutOfMemoryError.
     *
     * @param start The Expression from which to start the search.
     * @param rules A List of RewriteRules to use for generating the Expression graph.
     * @param isTarget A Function returning true if the given Expression is the desired result of the search.
     * @return Either an Expression satisfying the given predicate, or null if none is found.
     */
    public static Expression search(final Expression start, final List<RewriteRule> rules, final Function<Expression, Boolean> isTarget) {
        return BreadthFirstSearch.search(start,
                expr -> rules.stream().map(r -> {
                    System.out.println(r.rewrite(expr));
                    return r.rewrite(expr);
                }).collect(Collectors.toList()),
                isTarget);
    }
}
