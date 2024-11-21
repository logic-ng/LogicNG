package org.logicng.transformations.cnf;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.TestWithExampleFormulas.parse;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.predicates.CNFPredicate;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

/**
 * Unit tests for {@link CanonicalCNFEnumeration}.
 * @version 2.3.0
 * @since 2.3.0
 */
public class CanonicalCNFEnumerationTest {

    @Test
    public void testSamples() {
        final FormulaFactory f = new FormulaFactory();
        assertThat(f.falsum().transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "$false"));
        assertThat(f.verum().transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "$true"));
        assertThat(parse(f, "a").transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "a"));
        assertThat(parse(f, "~a").transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "~a"));
        assertThat(parse(f, "~a & b").transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "(~a | b) & (~a | ~b) & (a | b)"));
        assertThat(parse(f, "~a | b").transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "~a | b"));
        assertThat(parse(f, "a => b").transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "~a | b"));
        assertThat(parse(f, "a <=> b").transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "(~a | b) & (a | ~b)"));
        assertThat(parse(f, "a + b = 1").transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "(a | b) & (~a | ~b)"));
        assertThat(parse(f, "a & (b | ~c)").transform(CanonicalCNFEnumeration.get()))
                .isEqualTo(parse(f, "(a | b | c) & (a | b | ~c) & (a | ~b | c) & (a | ~b | ~c) & (~a | b | ~c)"));
        assertThat(parse(f, "a & b & (~a | ~b)").transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "(a | b) & (~a | b) & (~a | ~b) & (a | ~b)"));
        assertThat(parse(f, "a | b | ~a & ~b").transform(CanonicalCNFEnumeration.get())).isEqualTo(parse(f, "$true"));
    }

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        for (final Formula formula : cornerCases.cornerCases()) {
            test(formula);
        }
    }

    @Test
    @RandomTag
    public void random() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(5).weightPbc(0.5).seed(42).build());
        for (int i = 0; i < 1000; i++) {
            final Formula formula = randomizer.formula(3);
            test(formula);
        }
    }

    private void test(final Formula formula) {
        final FormulaFactory f = formula.factory();
        final Formula cnf = CanonicalCNFEnumeration.get().apply(formula, false);
        assertThat(cnf.holds(CNFPredicate.get())).isTrue();
        assertThat(f.equivalence(formula, cnf).holds(new TautologyPredicate(f))).isTrue();
        if (formula.holds(new TautologyPredicate(f))) {
            assertThat(cnf).isEqualTo(f.verum());
        } else {
            assertThat(hasConstantTermSize(cnf)).isTrue();
        }
    }

    private static boolean hasConstantTermSize(final Formula cnf) {
        switch (cnf.type()) {
            case LITERAL:
            case TRUE:
            case FALSE:
            case OR:
                return true;
            case AND:
                return cnf.stream().map(Formula::numberOfOperands).distinct().count() == 1L;
            default:
                throw new IllegalStateException("Unexpected type: " + cnf.type());
        }
    }

    @Test
    public void testToString() {
        assertThat(CanonicalCNFEnumeration.get().toString()).isEqualTo("CanonicalCNFEnumeration");
    }
}
