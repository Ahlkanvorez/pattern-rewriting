package patterns;

import java.util.List;

import static java.util.stream.Collectors.toList;
import static patterns.ExpressionParser.getTopLevelComponents;

public class PatternParser {
    public static boolean isVariable (final String s) {
        // TODO: Determine a better format for variable notation.
        return s.endsWith("?");
    }

    public static Pattern valueOf (final String s) {
        // If s starts with (, then it is a compound Pattern.
        if (s.startsWith("(")) {
            final List<String> values = getTopLevelComponents(s.trim() // Bounding whitespace is not counted.
                    .replaceFirst("^\\(", "") // Remove initial ( if present
                    .replaceFirst("\\)$", "")); // Remove final ) if present

            // Return an Expression of all the sub patterns.
            return Pattern.of(values.stream().map(PatternParser::valueOf).collect(toList()));
        }
        // if s does not start with (, then it is either a variable or a plain Pattern.
        return isVariable(s) ? Pattern.variableOf(s) : Pattern.of(s);
    }
}
