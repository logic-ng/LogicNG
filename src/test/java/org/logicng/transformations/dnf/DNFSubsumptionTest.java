package org.logicng.transformations.dnf;

import org.junit.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.readers.FormulaReader;
import org.logicng.predicates.satisfiability.TautologyPredicate;

import java.io.IOException;

import static org.assertj.core.api.Java6Assertions.assertThat;

/**
 * Unit tests for {@link DNFSubsumption}.
 *
 * @version 1.5.0
 * @since 1.5.0
 */
public class DNFSubsumptionTest {

    private final FormulaFactory f = new FormulaFactory();
    private final PropositionalParser p = new PropositionalParser(this.f);
    private final DNFSubsumption s = new DNFSubsumption();

    @Test(expected = IllegalArgumentException.class)
    public void testNotInDNF() throws ParserException {
        this.s.apply(this.p.parse("a => b"), false);
    }

    @Test
    public void testSimpleDNFSubsumption() throws ParserException {
        assertThat(this.s.apply(this.p.parse("$false"), false)).isEqualTo(this.p.parse("$false"));
        assertThat(this.s.apply(this.p.parse("$true"), false)).isEqualTo(this.p.parse("$true"));
        assertThat(this.s.apply(this.p.parse("a"), false)).isEqualTo(this.p.parse("a"));
        assertThat(this.s.apply(this.p.parse("~a"), false)).isEqualTo(this.p.parse("~a"));
        assertThat(this.s.apply(this.p.parse("a | b | c"), false)).isEqualTo(this.p.parse("a | b | c"));
        assertThat(this.s.apply(this.p.parse("a & b & c"), false)).isEqualTo(this.p.parse("a & b & c"));
        assertThat(this.s.apply(this.p.parse("a | (a & b)"), false)).isEqualTo(this.p.parse("a"));
        assertThat(this.s.apply(this.p.parse("(a & b) | (a & b & c)"), false)).isEqualTo(this.p.parse("a & b"));
        assertThat(this.s.apply(this.p.parse("a | (a & b) | (a & b & c)"), false)).isEqualTo(this.p.parse("a"));
        assertThat(this.s.apply(this.p.parse("a | (a & b) | b"), false)).isEqualTo(this.p.parse("a | b"));
        assertThat(this.s.apply(this.p.parse("a | (a & b) | c | (c & b)"), false)).isEqualTo(this.p.parse("a | c"));
        assertThat(this.s.apply(this.p.parse("(a & b) | (a & c) | (a & b & c)"), false)).isEqualTo(this.p.parse("(a & b) | (a & c)"));
    }

    @Test
    public void testLargeDNFSubsumption() throws ParserException {
        assertThat(this.s.apply(this.p.parse("(a & b & c & d) | (a & b & c & e) | (a & b & c)"), false)).isEqualTo(this.p.parse("(a & b & c)"));
        assertThat(this.s.apply(this.p.parse("(a & b) | (a & c) | (a & b & c) | (a & ~b & c) | (a & b & ~c) | (b & c)"), false)).isEqualTo(this.p.parse("(a & b) | (a & c) | (b & c)"));
        assertThat(this.s.apply(this.p.parse("(a & b) | (a & c) | (a & b & c) | (a & ~b & c) | (a & b & ~c) | (b & c)"), false)).isEqualTo(this.p.parse("(a & b) | (a & c) | (b & c)"));
        assertThat(this.s.apply(this.p.parse("a | ~b | (c & d) | (~a & ~b & ~c) | (b & c & d) | (a & b & c & d)"), false)).isEqualTo(this.p.parse("a | ~b | (c & d)"));
        assertThat(this.s.apply(this.p.parse("(a & b & c & d & e & f & g) | (b & d & f) | (a & c & e & g)"), false)).isEqualTo(this.p.parse("(b & d & f) | (a & c & e & g)"));
    }

    @Test
    public void testEvenLargerFormulas() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/small_formulas.txt", f);
        int count = 10; // test only first 10 formulas
        for (final Formula op : formula) {
            if (count == 0) {
                break;
            }
            final Formula dnf = op.transform(new DNFFactorization());
            final Formula subsumed = dnf.transform(new DNFSubsumption());
            assertThat(f.equivalence(dnf, subsumed).holds(new TautologyPredicate(f))).isTrue();
            assertThat(dnf.numberOfOperands() > subsumed.numberOfOperands()).isTrue();
            count--;
        }
    }
}
