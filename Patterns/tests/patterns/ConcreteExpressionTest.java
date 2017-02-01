package patterns;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Robert Mitchell <robert.mitchell36@gmail.com>
 */
class ConcreteExpressionTest {
    @BeforeEach
    void setUp() {

    }

    @Test
    void testExpressionScalarInstantiation() {
        assert Expression.of("This is a scalar") != null;
    }

    @Test
    void testExpressionTreeInstantiation() {

    }

    @Test
    void testValue() {

    }

    @Test
    void testSubExpressions() {

    }

    @Test
    void testEquals() {

    }

    @Test
    void testHashCode() {

    }

    @Test
    void testToString() {

    }

}