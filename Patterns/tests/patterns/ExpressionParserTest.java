package patterns;

import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

class ExpressionParserTest {

    @Test
    void testSimpleExpression () {
        final String str = "a";

        final Expression exp = ExpressionParser.valueOf(str);

        assertEquals(Expression.of(str), exp);
    }

    @Test
    void testSingletonTreeExpression () {
        final String str = "(a)";

        final Expression exp = ExpressionParser.valueOf(str);

        assertEquals(Expression.of(Arrays.asList(
                    Expression.of("a")
                )),
                exp
        );
    }

    @Test
    void testTreeExpression () {
        final String a = "a";
        final String b = "b";
        final String c = "c";
        final String str = String.format("(%s %s %s)", a, b, c);

        final Expression exp = ExpressionParser.valueOf(str);

        assertEquals(
                Expression.of(Arrays.asList(Expression.of(a), Expression.of(b), Expression.of(c))),
                exp
        );
    }

    @Test
    void testMultiLevelTree () {
        final String a = "a";
        final String b = "b";
        final String c = "c";
        final String d = "d";
        final String e = "e";
        final String str = String.format("(%s ((%s %s) %s) %s)", a, b, c, d, e);

        final Expression exp = ExpressionParser.valueOf(str);

        assertEquals(
                Expression.of(Arrays.asList(
                        Expression.of(a),
                        Expression.of(Arrays.asList(
                                Expression.of(Arrays.asList(
                                        Expression.of(b),
                                        Expression.of(c)
                                )),
                                Expression.of(d)
                        )),
                        Expression.of(e)
                )),
                exp
        );
    }
}