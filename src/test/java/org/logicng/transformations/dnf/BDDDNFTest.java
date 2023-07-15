// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.dnf;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.testutils.TestUtil.equivalentModels;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.predicates.DNFPredicate;

/**
 * Unit Tests for {@link BDDDNFTransformation}.
 * @version 2.4.2
 * @since 2.3.0
 */
public class BDDDNFTest extends TestWithExampleFormulas {

    private final BDDDNFTransformation bdddnf = new BDDDNFTransformation();
    private final DNFPredicate dnfPredicate = DNFPredicate.get();

    @Test
    public void testConstants() {
        assertThat(this.TRUE.transform(this.bdddnf)).isEqualTo(this.TRUE);
        assertThat(this.FALSE.transform(this.bdddnf)).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.transform(this.bdddnf)).isEqualTo(this.A);
        assertThat(this.NA.transform(this.bdddnf)).isEqualTo(this.NA);
    }

    @Test
    public void testBinaryOperators() {
        assertThat(this.IMP1.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(this.IMP1, this.IMP1.transform(this.bdddnf), this.IMP1.variables())).isTrue();
        assertThat(this.IMP2.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(this.IMP2, this.IMP2.transform(this.bdddnf), this.IMP2.variables())).isTrue();
        assertThat(this.IMP3.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(this.IMP3, this.IMP3.transform(this.bdddnf), this.IMP3.variables())).isTrue();
        assertThat(this.EQ1.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ1, this.EQ1.transform(this.bdddnf), this.EQ1.variables())).isTrue();
        assertThat(this.EQ2.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ2, this.EQ2.transform(this.bdddnf), this.EQ2.variables())).isTrue();
        assertThat(this.EQ3.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ3, this.EQ3.transform(this.bdddnf), this.EQ3.variables())).isTrue();
        assertThat(this.EQ4.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ4, this.EQ4.transform(this.bdddnf), this.EQ4.variables())).isTrue();
    }

    @Test
    public void testNAryOperators() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        assertThat(this.AND1.transform(this.bdddnf)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.bdddnf)).isEqualTo(this.OR1);
        final Formula f1 = p.parse("~(a | b) & c & ~(x & ~y) & (w => z)");
        final Formula f2 = p.parse("~(a & b) | c | ~(x | ~y)");
        final Formula f3 = p.parse("a | b | (~x & ~y)");
        assertThat(f1.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.bdddnf), f1.variables())).isTrue();
        assertThat(f2.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.bdddnf), f2.variables())).isTrue();
        assertThat(f3.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.bdddnf), f3.variables())).isTrue();
    }

    @Test
    public void testNAryOperatorsWithExternalFactory() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final BDDDNFTransformation transformation = new BDDDNFTransformation(f, 7);
        assertThat(this.AND1.transform(this.bdddnf)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.bdddnf)).isEqualTo(this.OR1);
        final Formula f1 = p.parse("~(a | b) & c & ~(x & ~y) & (w => z)");
        final Formula f2 = p.parse("~(a & b) | c | ~(x | ~y)");
        final Formula f3 = p.parse("a | b | (~x & ~y)");
        assertThat(f1.transform(transformation).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(transformation), f1.variables())).isTrue();
        assertThat(f2.transform(transformation).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(transformation), f2.variables())).isTrue();
        assertThat(f3.transform(transformation).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(transformation), f3.variables())).isTrue();
    }

    @Test
    public void testNAryOperatorsWithExternalFactory2() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final BDDDNFTransformation transformation = new BDDDNFTransformation(new BDDKernel(f, 7, 50, 50));
        assertThat(this.AND1.transform(this.bdddnf)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.bdddnf)).isEqualTo(this.OR1);
        final Formula f1 = p.parse("~(a | b) & c & ~(x & ~y) & (w => z)");
        final Formula f2 = p.parse("~(a & b) | c | ~(x | ~y)");
        final Formula f3 = p.parse("a | b | (~x & ~y)");
        assertThat(f1.transform(transformation).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(transformation), f1.variables())).isTrue();
        assertThat(f2.transform(transformation).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(transformation), f2.variables())).isTrue();
        assertThat(f3.transform(transformation).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(transformation), f3.variables())).isTrue();
    }

    @Test
    public void testNot() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(p.parse("~a").transform(this.bdddnf)).isEqualTo(p.parse("~a"));
        assertThat(p.parse("~~a").transform(this.bdddnf)).isEqualTo(p.parse("a"));
        assertThat(p.parse("~(a => b)").transform(this.bdddnf)).isEqualTo(p.parse("a & ~b"));
        final Formula f1 = p.parse("~(~(a | b) => ~(x | y))");
        final Formula f2 = p.parse("~(a <=> b)");
        final Formula f3 = p.parse("~(~(a | b) <=> ~(x | y))");
        final Formula f4 = p.parse("~(a & b & ~x & ~y)");
        final Formula f5 = p.parse("~(a | b | ~x | ~y)");
        assertThat(f1.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.bdddnf), f1.variables())).isTrue();
        assertThat(f2.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.bdddnf), f2.variables())).isTrue();
        assertThat(f3.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.bdddnf), f3.variables())).isTrue();
        assertThat(f4.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f4, f4.transform(this.bdddnf), f4.variables())).isTrue();
        assertThat(f5.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f5, f5.transform(this.bdddnf), f5.variables())).isTrue();
        assertThat(f5.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f5, f5.transform(this.bdddnf), f5.variables())).isTrue();
    }

    @Test
    public void testCC() throws ParserException {
        final PseudoBooleanParser p = new PseudoBooleanParser(this.f);
        final Formula f1 = p.parse("a <=> (1 * b <= 0)");
        final Formula f2 = p.parse("~(1 * b <= 1)");
        final Formula f3 = p.parse("(1 * b + 1 * c + 1 * d <= 1)");
        final Formula f4 = p.parse("~(1 * b + 1 * c + 1 * d <= 1)");
        assertThat(f1.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.bdddnf), f1.variables())).isTrue();
        assertThat(f2.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.bdddnf), f2.variables())).isTrue();
        assertThat(f3.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.bdddnf), f3.variables())).isTrue();
        assertThat(f4.transform(this.bdddnf).holds(this.dnfPredicate)).isTrue();
        assertThat(equivalentModels(f4, f4.transform(this.bdddnf), f4.variables())).isTrue();
    }
}
