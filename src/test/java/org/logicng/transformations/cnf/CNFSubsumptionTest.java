package org.logicng.transformations.cnf;

import static org.assertj.core.api.Java6Assertions.assertThat;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.readers.FormulaReader;
import org.logicng.predicates.satisfiability.TautologyPredicate;

import java.io.IOException;

/**
 * Unit tests for {@link CNFSubsumption}.
 * @version 1.5.0
 * @since 1.5.0
 */
public class CNFSubsumptionTest {

    private final FormulaFactory f = new FormulaFactory();
    private final PropositionalParser p = new PropositionalParser(this.f);
    private final CNFSubsumption s = new CNFSubsumption();

    @Test(expected = IllegalArgumentException.class)
    public void testNotInCNF() throws ParserException {
        this.s.apply(this.p.parse("a => b"), false);
    }

    @Test
    public void testSimpleCNFSubsumption() throws ParserException {
        Assert.assertEquals(this.s.apply(this.p.parse("$false"), false), this.p.parse("$false"));
        Assert.assertEquals(this.s.apply(this.p.parse("$true"), false), this.p.parse("$true"));
        Assert.assertEquals(this.s.apply(this.p.parse("a"), false), this.p.parse("a"));
        Assert.assertEquals(this.s.apply(this.p.parse("~a"), false), this.p.parse("~a"));
        Assert.assertEquals(this.s.apply(this.p.parse("a | b | c"), false), this.p.parse("a | b | c"));
        Assert.assertEquals(this.s.apply(this.p.parse("a & b & c"), false), this.p.parse("a & b & c"));
        Assert.assertEquals(this.s.apply(this.p.parse("a & (a | b)"), false), this.p.parse("a"));
        Assert.assertEquals(this.s.apply(this.p.parse("(a | b) & (a | b | c)"), false), this.p.parse("a | b"));
        Assert.assertEquals(this.s.apply(this.p.parse("a & (a | b) & (a | b | c)"), false), this.p.parse("a"));
        Assert.assertEquals(this.s.apply(this.p.parse("a & (a | b) & b"), false), this.p.parse("a & b"));
        Assert.assertEquals(this.s.apply(this.p.parse("a & (a | b) & c & (c | b)"), false), this.p.parse("a & c"));
        Assert.assertEquals(this.s.apply(this.p.parse("(a | b) & (a | c) & (a | b | c)"), false), this.p.parse("(a | b) & (a | c)"));
    }

    @Test
    public void testLargeCNFSubsumption() throws ParserException {
        Assert.assertEquals(this.s.apply(this.p.parse("(a | b | c | d) & (a | b | c | e) & (a | b | c)"), false), this.p.parse("(a | b | c)"));
        Assert.assertEquals(this.s.apply(this.p.parse("(a | b) & (a | c) & (a | b | c) & (a | ~b | c) & (a | b | ~c) & (b | c)"), false), this.p.parse("(a | b) & (a | c) & (b | c)"));
        Assert.assertEquals(this.s.apply(this.p.parse("(a | b) & (a | c) & (a | b | c) & (a | ~b | c) & (a | b | ~c) & (b | c)"), false), this.p.parse("(a | b) & (a | c) & (b | c)"));
        Assert.assertEquals(this.s.apply(this.p.parse("a & ~b & (c | d) & (~a | ~b | ~c) & (b | c | d) & (a | b | c | d)"), false), this.p.parse("a & ~b & (c | d)"));
        Assert.assertEquals(this.s.apply(this.p.parse("(a | b | c | d | e | f | g) & (b | d | f) & (a | c | e | g)"), false), this.p.parse("(b | d | f) & (a | c | e | g)"));
    }

    @Test
    public void testEvenLargerFormula() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final Formula formula = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/large_formula.txt", f);
        final Formula cnf = formula.transform(new CNFFactorization());
        final Formula subsumed = cnf.transform(new CNFSubsumption());
        assertThat(f.equivalence(cnf, subsumed).holds(new TautologyPredicate(f))).isTrue();
        assertThat(cnf.numberOfOperands() > subsumed.numberOfOperands()).isTrue();
    }
}
