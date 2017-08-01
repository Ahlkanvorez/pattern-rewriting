package patterns;

import com.sun.javaws.exceptions.InvalidArgumentException;

import java.util.ArrayList;
import java.util.List;

import static java.util.stream.Collectors.toList;

public class ExpressionParser {

    static List<String> getTopLevelComponents (final String s) {
        final List<String> values = new ArrayList<>();
        int start = 0;
        int parensDepth = 0;
        for (int i = 0; i < s.length(); ++i) {
            switch (s.charAt(i)) {
                case '(':
                    parensDepth++;
                    break;
                case ')':
                    parensDepth--;
                    break;
                case ' ':
                    if (parensDepth == 0) {
                        values.add(s.substring(start, i));
                        start = i + 1;
                    }
                    break;
            }
        }
        if (parensDepth > 0) {
            throw new IllegalArgumentException("Invalid expression: unbalanced parentheses.");
        }
        // If the start point isn't at the end of the string, then a substring has not been added, so add it.
        if (start != s.length()) {
            values.add(s.substring(start));
        }
        return values;
    }

    /** Parses an expression of the form (expr ...subexpr), where expr is the expression, and ...subexpr is a
     * space-separated list of sub expressions, which may or may not themselves be parenthetically wrapped expressions.
     *
     * @param s the String form of the expression to parse.
     * @return an Expression instance represented by the provided String.
     */
    public static Expression valueOf (final String s) {
        if (s.startsWith("(")) {
            final List<String> values = getTopLevelComponents(s.trim() // Bounding whitespace is not counted.
                    .replaceFirst("^\\(", "") // Remove initial ( if present
                    .replaceFirst("\\)$", "")); // Remove final ) if present

            // Return an Expression of all the sub expressions.
            return Expression.of(values.stream().map(ExpressionParser::valueOf).collect(toList()));
        }
        // if s does not start with (, then it is a plain expression.
        return Expression.of(s);
    }
}
