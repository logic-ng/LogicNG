package org.logicng.io.readers;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;

import java.io.File;

/**
 * Unit Tests for the class {@link DimacsReader}.
 * @version 2.0.0
 * @since 1.0
 */
public class DimacsReaderTest {

    @Test
    public void testExceptionalBehavior() {
        assertThatThrownBy(() -> {
            final FormulaFactory f = new FormulaFactory();
            final File file = new File("src/test/resources/dimacs/malformed/contains-line-without-zero.cnf");
            DimacsReader.readCNF(file, f, "v");
        }).isInstanceOf(IllegalArgumentException.class)
                .hasMessage("Line '2 -3' did not end with 0.");
    }
}
