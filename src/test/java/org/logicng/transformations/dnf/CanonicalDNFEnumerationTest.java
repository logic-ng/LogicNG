package org.logicng.transformations.dnf;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.TestWithExampleFormulas.parse;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.predicates.DNFPredicate;
import org.logicng.predicates.satisfiability.ContradictionPredicate;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

/**
 * Unit tests for {@link CanonicalDNFEnumeration}.
 * @version 2.3.0
 * @since 2.3.0
 */
public class CanonicalDNFEnumerationTest {

    @Test
    public void testSamples() {
        final FormulaFactory f = new FormulaFactory();
        assertThat(f.falsum().transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "$false"));
        assertThat(f.verum().transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "$true"));
        assertThat(parse(f, "a").transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "a"));
        assertThat(parse(f, "~a").transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "~a"));
        assertThat(parse(f, "~a & b").transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "~a & b"));
        assertThat(parse(f, "~a | b").transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "~a & ~b | ~a & b | a & b"));
        assertThat(parse(f, "a => b").transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "~a & ~b | ~a & b | a & b"));
        assertThat(parse(f, "a <=> b").transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "a & b | ~a & ~b"));
        assertThat(parse(f, "a + b = 1").transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "~a & b | a & ~b"));
        assertThat(parse(f, "a & (b | ~c)").transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "a & b & c | a & b & ~c | a & ~b & ~c"));
        assertThat(parse(f, "a & b & (~a | ~b)").transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "$false"));
        assertThat(parse(f, "a | b | ~a & ~b").transform(CanonicalDNFEnumeration.get())).isEqualTo(parse(f, "~a & b | a & b | a & ~b | ~a & ~b"));
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
        final Formula dnf = CanonicalDNFEnumeration.get().apply(formula, false);
        assertThat(dnf.holds(DNFPredicate.get())).isTrue();
        assertThat(f.equivalence(formula, dnf).holds(new TautologyPredicate(f))).isTrue();
        if (formula.holds(new ContradictionPredicate(f))) {
            assertThat(dnf).isEqualTo(f.falsum());
        } else {
            assertThat(hasConstantTermSize(dnf)).isTrue();
        }
    }

    private static boolean hasConstantTermSize(final Formula dnf) {
        switch (dnf.type()) {
            case LITERAL:
            case TRUE:
            case FALSE:
            case AND:
                return true;
            case OR:
                return dnf.stream().map(Formula::numberOfOperands).distinct().count() == 1;
            default:
                throw new IllegalStateException("Unexpected type: " + dnf.type());
        }
    }

    @Test
    public void testToString() {
        assertThat(CanonicalDNFEnumeration.get().toString()).isEqualTo("CanonicalDNFEnumeration");
    }
}
