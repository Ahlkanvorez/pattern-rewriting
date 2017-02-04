package patterns;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * A Rewrite-Rule is the function which transforms an Expression matching one Pattern to an Expression matching another.
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
 * A RewriteRule is made up of two Patterns, one specifying the possible input Expressions to be transformed (viz. those
 * Expressions which match that Pattern), and the other specifying the possible output Expressions to be transformed to.
 *
 * A RewriteRule is Immutable.
 *
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
public final class RewriteRule {
    private final Pattern domainPattern;
    private final Pattern rangePattern;

    /** Instantiates a RewriteRule using the two given Patterns specifying the valid input and output Expressions.
     * Note: This constructor is private to force the use of the static factory method, which caches RewriteRule
     * instances. If an instance is desired but it is not desired that it be cached, the static factory method
     * ruleWithoutCachingFrom(...) should be used.
     *
     * @param domain A Pattern which specifies the valid input Expressions. The set of possible inputs is the set of
     *               Expressions which this Pattern can successfully match.
     * @param range A Pattern which specifies the valid output Expressions. The set of possible outputs is the set of
     *              Expressions which this Pattern can successfully match.
     */
    private RewriteRule(final Pattern domain, final Pattern range) {
        this.domainPattern = Objects.requireNonNull(domain, "A Rewrite-Rule cannot match with a null pattern");
        this.rangePattern = Objects.requireNonNull(range, "A Rewrite-Rule cannot rewrite to a null pattern");
    }

    /** The underlying Pattern for this RewriteRule which every input of .rewrite(...) must match successfully.
     * This pattern partitions the set of all Expressions by specifying a set of Expressions which can be passed to
     * the .rewrite(...) method of this RewriteRule, and designating all other Expressions as invalid inputs.
     *
     * @return The underlying Pattern from which Expressions are rewritten.
     */
    public Pattern domainPattern() {
        return this.domainPattern;
    }

    /** The underlying Pattern for this RewriteRule which every output of .rewrite(...) will match successfully.
     * This Pattern partitions the set of all Expressions by specifying a set of Expressions that can result from a call
     * to .rewrite(...), and by extension, designating any Expression which it does not match as unreachable using this
     * RewriteRule.
     *
     * @return The underlying Pattern to which Expressions are rewritten.
     */
    public Pattern rangePattern() {
        return this.rangePattern;
    }

    /** This method returns true if the given Expression exists in the set of possible inputs for .rewrite(...), which
     * is the set of Expressions that domainPattern can successfully match, and false otherwise.
     *
     * @param e The Expression to check for being a valid input to .match(...) for this RewriteRule.
     * @return true if the given Expression can be rewritten using this RewriteRule, false otherwise.
     */
    public boolean canRewrite(Expression e) {
        return this.domainPattern.matches(e);
    }

    /** This method returns true if the given Expression exists in the set of possible outputs of .rewrite(...), which
     * is the set of Expressions that rangePattern can successfully match, and false otherwise.
     *
     * @param e The Expression to check for being matchable with the rangePattern of this RewriteRule.
     * @return true if the given Expression is a possible output of .rewrite(_) using this RewriteRule, otherwise false.
     */
    public boolean canRewriteTo(Expression e) {
        return this.rangePattern.matches(e);
    }

    /** Returns a new Expression, which if given to an invocation of .matches(_) on the rangePattern of this RewriteRule
     * will result in true, and which originates it's contents from the contents of the given expression by binding with
     * the domainPattern of this RewriteRule.
     *
     * @param e The Expression to rewrite from the domainPattern to the rangePattern.
     * @return A new Expression based on the given Expression which matches the rangePattern of this RewriteRule.
     */
    public Expression rewrite(Expression e) {
        if (!this.canRewrite(e)) {
            System.err.println("<" + this + "> Cannot rewrite Expression: " + e);
            return null;
        }
        return rangePattern.expressionFrom(domainPattern.match(e));
    }

    /** An object is equal to a RewriteRule instance if
     *
     * Because RewriteRules are cached unless otherwise specified, a simple == test is more efficient and preferable.
     * However, supposing an uncached instance were being compared against, this method would return true if both
     * instances had equal Patterns.
     *
     * @param other The other RewriteRule to test for equality against.
     * @return true if both RewriteRules have the same underlying Patterns in the same positions, false otherwise.
     */
    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || !(other instanceof RewriteRule)) {
            return false;
        }
        RewriteRule o = (RewriteRule) other;
        return this.domainPattern.equals(o.domainPattern) && this.rangePattern.equals(o.rangePattern);
    }

    /** A Unique integer representing this RewriteRule instance.
     *
     * @return A Unique integer calculated from the hashCode() outputs of the Patterns of this RewriteRule.
     */
    @Override
    public int hashCode() {
        int hash = 31;
        hash = hash * 17 + this.domainPattern.hashCode();
        hash = hash * 17 + this.rangePattern.hashCode();
        return hash;
    }

    /** Returns a String representation of a RewriteRule.
     * RewriteRules are represented as Strings in the following format:
     * (domainPattern.toString() -> rangePattern.toString())
     *
     * @return A String representing the RewriteRule in terms of it's Patterns' .toString() outputs.
     */
    @Override
    public String toString() {
        return String.format("(%s -> %s)", domainPattern.toString(), rangePattern.toString());
    }

    /**
     * This map of maps caches RewriteRule instances according to their domain and range Patterns; the first key being
     * the domain, and the second key being the range Pattern. Any instance created through the static factory method
     * ruleFrom(...) will be cached in this map, or if it is already present, retrieved and returned from the map
     * without instantiating a new object. If an instance is desired without adding a new instance to the cache, the
     * static factory method ruleWithoutCachingFrom(...) should be used.
     */
    private static final Map<Pattern, Map<Pattern, RewriteRule>> cache = new HashMap<>();

    /**
     * Given two Patterns, this static factory method either instantiates a new RewriteRule if a rule with those two
     * Patterns does not exist in the cache, subsequently adding it to the cache before returning it, or it retrieves
     * and returns an existing RewriteRule from the cache with the specified domain and range Patterns. If a RewriteRule
     * is desired, but caching is undesired, use the static factory method ruleWithoutCachingFrom(...).
     *
     * @param domainPattern A Pattern which specifies the valid input Expressions. The set of possible inputs is the set
     *                      of Expressions which this Pattern can successfully match.
     * @param rangePattern A Pattern which specifies the valid output Expressions. The set of possible outputs is the
     *                     set of Expressions which this Pattern can successfully match.
     * @return A RewriteRule Instance from the cache with the specified Patterns.
     */
    public static RewriteRule ruleFrom(final Pattern domainPattern, final Pattern rangePattern) {
        if (!cache.containsKey(domainPattern)) {
            cache.put(domainPattern, new HashMap<>());
        }
        if (!cache.get(domainPattern).containsKey(rangePattern)) {
            cache.get(domainPattern).put(rangePattern, new RewriteRule(domainPattern, rangePattern));
        }
        return cache.get(domainPattern).get(rangePattern);
    }

    /**
     * Given two Patterns, this static factory method either instantiates a new RewriteRule if a rule with those two
     * Patterns does not exist in the cache, not adding it to the cache before returning it, or it retrieves
     * and returns an existing RewriteRule from the cache with the specified domain and range Patterns. If a RewriteRule
     * is desired, as well as adding it to the cache, use the static factory method ruleFrom(...).
     *
     * @param domainPattern A Pattern which specifies the valid input Expressions. The set of possible inputs is the set
     *                      of Expressions which this Pattern can successfully match.
     * @param rangePattern A Pattern which specifies the valid output Expressions. The set of possible outputs is the
     *                     set of Expressions which this Pattern can successfully match.
     * @return A RewriteRule Instance with the specified Patterns which will not be added to the cache.
     */
    public static RewriteRule ruleWithoutCachingFrom(final Pattern domainPattern, final Pattern rangePattern) {
        if (cache.containsKey(domainPattern) && cache.get(domainPattern).containsKey(rangePattern)) {
            return cache.get(domainPattern).get(rangePattern);
        }
        return new RewriteRule(domainPattern, rangePattern);
    }
}
