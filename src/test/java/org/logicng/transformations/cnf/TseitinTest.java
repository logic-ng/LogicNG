// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.cnf;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.List;
import java.util.SortedSet;

/**
 * Unit Tests for {@link TseitinTransformation}.
 * @version 2.3.0
 * @since 1.0
 */
public class TseitinTest extends TestWithExampleFormulas {

    private final TseitinTransformation ts = new TseitinTransformation(0);

    @Test
    public void testConstants() {
        assertThat(this.TRUE.transform(this.ts)).isEqualTo(this.TRUE);
        assertThat(this.FALSE.transform(this.ts)).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.transform(this.ts)).isEqualTo(this.A);
        assertThat(this.NA.transform(this.ts)).isEqualTo(this.NA);
    }

    @Test
    public void testBinaryOperators() {
        assertThat(this.IMP1.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(this.IMP1, this.IMP1.transform(this.ts), this.IMP1.variables())).isTrue();
        assertThat(this.IMP2.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(this.IMP2, this.IMP2.transform(this.ts), this.IMP2.variables())).isTrue();
        assertThat(this.IMP3.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(this.IMP3, this.IMP3.transform(this.ts), this.IMP3.variables())).isTrue();
        assertThat(this.EQ1.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(this.EQ1, this.EQ1.transform(this.ts), this.EQ1.variables())).isTrue();
        assertThat(this.EQ2.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(this.EQ2, this.EQ2.transform(this.ts), this.EQ2.variables())).isTrue();
        assertThat(this.EQ3.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(this.EQ3, this.EQ3.transform(this.ts), this.EQ3.variables())).isTrue();
        assertThat(this.EQ4.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(this.EQ4, this.EQ4.transform(this.ts), this.EQ4.variables())).isTrue();
    }

    @Test
    public void testNAryOperators() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.AND1.transform(this.ts)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.ts)).isEqualTo(this.OR1);
        final Formula f1 = p.parse("~(a | b) & c & ~(x & ~y) & (w => z)");
        final Formula f2 = p.parse("~(a & b) | c | ~(x | ~y)");
        final Formula f3 = p.parse("a | b | (~x & ~y)");
        assertThat(f1.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.ts), f1.variables())).isTrue();
        assertThat(f2.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.ts), f2.variables())).isTrue();
        assertThat(f3.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.ts), f3.variables())).isTrue();
    }

    @Test
    public void testNot() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(p.parse("~a").transform(this.ts)).isEqualTo(p.parse("~a"));
        assertThat(p.parse("~~a").transform(this.ts)).isEqualTo(p.parse("a"));
        assertThat(p.parse("~(a => b)").transform(this.ts)).isEqualTo(p.parse("a & ~b"));
        final Formula f1 = p.parse("~(~(a | b) => ~(x | y))");
        final Formula f2 = p.parse("~(a <=> b)");
        final Formula f3 = p.parse("~(~(a | b) <=> ~(x | y))");
        final Formula f4 = p.parse("~(a & b & ~x & ~y)");
        final Formula f5 = p.parse("~(a | b | ~x | ~y)");
        assertThat(f1.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.ts), f1.variables())).isTrue();
        assertThat(f2.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.ts), f2.variables())).isTrue();
        assertThat(f3.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.ts), f3.variables())).isTrue();
        assertThat(f4.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(f4, f4.transform(this.ts), f4.variables())).isTrue();
        assertThat(f5.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(f5, f5.transform(this.ts), f5.variables())).isTrue();
        assertThat(f5.transform(this.ts).isCNF()).isTrue();
        assertThat(equivalentModels(f5, f5.transform(this.ts), f5.variables())).isTrue();
    }

    @Test
    public void testFactorization() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        final TseitinTransformation pgf = new TseitinTransformation();
        final Formula f1 = p.parse("(a | b) => c");
        final Formula f2 = p.parse("~x & ~y");
        final Formula f3 = p.parse("d & ((a | b) => c)");
        final Formula f4 = p.parse("d & ((a | b) => c) | ~x & ~y");
        assertThat(f1.transform(pgf).isCNF()).isTrue();
        assertThat(f1.transform(pgf).variables().size()).isEqualTo(f1.variables().size());
        assertThat(f2.transform(pgf).isCNF()).isTrue();
        assertThat(f2.transform(pgf).variables().size()).isEqualTo(f2.variables().size());
        assertThat(f3.transform(pgf).isCNF()).isTrue();
        assertThat(f3.transform(pgf).variables().size()).isEqualTo(f3.variables().size());
        assertThat(f4.transform(pgf).isCNF()).isTrue();
        assertThat(f4.transform(pgf).variables().size()).isEqualTo(f4.variables().size());
    }

    @Test
    public void testCC() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        assertThat(p.parse("a <=> (1 * b <= 1)").transform(this.ts)).isEqualTo(p.parse("a"));
        assertThat(p.parse("~(1 * b <= 1)").transform(this.ts)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("(1 * b + 1 * c + 1 * d <= 1)").transform(this.ts))
                .isEqualTo(p.parse("(~b | ~c) & (~b | ~d) & (~c | ~d)"));
        assertThat(p.parse("~(1 * b + 1 * c + 1 * d <= 1)").transform(this.ts)).isEqualTo(p.parse(
                "(d | @RESERVED_CC_1 | @RESERVED_CC_4) & (~@RESERVED_CC_3 | @RESERVED_CC_1 | @RESERVED_CC_4) & (~@RESERVED_CC_3 | d | @RESERVED_CC_4) & (~@RESERVED_CC_4 | @RESERVED_CC_0) & (~@RESERVED_CC_2 | @RESERVED_CC_0) & (~@RESERVED_CC_4 | ~@RESERVED_CC_2) & (c | @RESERVED_CC_3 | @RESERVED_CC_5) & (b | @RESERVED_CC_3 | @RESERVED_CC_5) & (b | c | @RESERVED_CC_5) & (~@RESERVED_CC_5 | @RESERVED_CC_2) & ~@RESERVED_CC_0"));
    }

    @Test
    public void testToString() {
        final TseitinTransformation tseitinTransformation = new TseitinTransformation(5);
        assertThat(tseitinTransformation.toString()).isEqualTo("TseitinTransformation{boundary=5}");
    }

    private boolean equivalentModels(final Formula f1, final Formula f2, final SortedSet<Variable> vars) {
        final SATSolver s = MiniSat.miniSat(f1.factory());
        s.add(f1);
        final List<Assignment> models1 = s.enumerateAllModels(vars);
        s.reset();
        s.add(f2);
        final List<Assignment> models2 = s.enumerateAllModels(vars);
        if (models1.size() != models2.size()) {
            return false;
        }
        for (final Assignment model : models1) {
            if (!models2.contains(model)) {
                return false;
            }
        }
        return true;
    }
}
