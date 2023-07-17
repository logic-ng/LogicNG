// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.simplification;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

/**
 * Unit tests for the class {@link NegationSimplifier}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class NegationMinimizerTest extends TestWithExampleFormulas {

    private final NegationSimplifier minimizer = NegationSimplifier.get();

    @Test
    public void testSimple() throws ParserException {
        assertThat(this.f.falsum().transform(this.minimizer)).isEqualTo(this.f.falsum());
        assertThat(this.f.verum().transform(this.minimizer)).isEqualTo(this.f.verum());
        assertThat(this.A.transform(this.minimizer)).isEqualTo(this.A);
        assertThat(this.NA.transform(this.minimizer)).isEqualTo(this.NA);

        assertThat(this.f.parse("A&~B&~C&~D").transform(this.minimizer)).isEqualTo(this.f.parse("A&~B&~C&~D"));
        assertThat(this.f.parse("~A&~B&~C&~D").transform(this.minimizer)).isEqualTo(this.f.parse("~(A|B|C|D)"));

        assertThat(this.f.parse("A|~B|~C|~D").transform(this.minimizer)).isEqualTo(this.f.parse("A|~B|~C|~D"));
        assertThat(this.f.parse("~A|~B|~C|~D").transform(this.minimizer)).isEqualTo(this.f.parse("~(A&B&C&D)"));

        assertThat(this.f.parse("~A|~B|~C|D|~E|~G").transform(this.minimizer))
                .isEqualTo(this.f.parse("D|~(A&B&C&E&G)"));
        assertThat(this.f.parse("~A&~B&~C&D&~E&~G").transform(this.minimizer))
                .isEqualTo(this.f.parse("D&~(A|B|C|E|G)"));

        assertThat(this.f.parse("~A|~B|~E&G|~H&~B&~C|~X").transform(this.minimizer))
                .isEqualTo(this.f.parse("~E&G|~(A&B&(H|B|C)&X)"));
        assertThat(this.f.parse("~(A&B&~(~E&G)&(H|B|C)&X)").transform(this.minimizer))
                .isEqualTo(this.f.parse("~E&G|~(A&B&(H|B|C)&X)"));

        assertThat(this.f.parse("~A|B|(~E&~G&~H&~K)").transform(this.minimizer))
                .isEqualTo(this.f.parse("~A|B|~(E|G|H|K)"));

        assertThat(this.f.parse("~A|~B").transform(this.minimizer)).isEqualTo(this.f.parse("~A|~B"));
        assertThat(this.f.parse("~A|~B|~C").transform(this.minimizer)).isEqualTo(this.f.parse("~A|~B|~C"));
        assertThat(this.f.parse("~A|~B|~C|~D").transform(this.minimizer)).isEqualTo(this.f.parse("~(A&B&C&D)"));

        assertThat(this.f.parse("X&(~A|~B)").transform(this.minimizer)).isEqualTo(this.f.parse("X&~(A&B)"));
        assertThat(this.f.parse("X&(~A|~B|~C)").transform(this.minimizer)).isEqualTo(this.f.parse("X&~(A&B&C)"));
        assertThat(this.f.parse("X&(~A|~B|~C|~D)").transform(this.minimizer)).isEqualTo(this.f.parse("X&~(A&B&C&D)"));

        assertThat(this.f.parse("~A&~B").transform(this.minimizer)).isEqualTo(this.f.parse("~A&~B"));
        assertThat(this.f.parse("~A&~B&~C").transform(this.minimizer)).isEqualTo(this.f.parse("~A&~B&~C"));
        assertThat(this.f.parse("~A&~B&~C&~D").transform(this.minimizer)).isEqualTo(this.f.parse("~(A|B|C|D)"));

        assertThat(this.f.parse("X|~A&~B").transform(this.minimizer)).isEqualTo(this.f.parse("X|~A&~B"));
        assertThat(this.f.parse("X|~A&~B&~C").transform(this.minimizer)).isEqualTo(this.f.parse("X|~A&~B&~C"));
        assertThat(this.f.parse("X|~A&~B&~C&~D").transform(this.minimizer)).isEqualTo(this.f.parse("X|~(A|B|C|D)"));

        assertThat(this.f.parse("A&(~B|~C|~D|~E|~G|X|Y|H)").transform(this.minimizer))
                .isEqualTo(this.f.parse("A&(~(B&C&D&E&G)|X|Y|H)"));
    }

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        cornerCases.cornerCases().forEach(NegationMinimizerTest::computeAndVerify);
    }

    @Test
    @RandomTag
    public void testRandomized() {
        for (int i = 0; i < 1000; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f,
                    FormulaRandomizerConfig.builder().numVars(5).weightPbc(2).seed(i * 42).build());
            computeAndVerify(randomizer.formula(6));
        }
    }

    private static void computeAndVerify(final Formula formula) {
        final FormulaFactory f = formula.factory();
        final Formula simplified = formula.transform(NegationSimplifier.get());
        assertThat(f.equivalence(formula, simplified).holds(new TautologyPredicate(f))).isTrue();
        assertThat(simplified.toString().length()).isLessThanOrEqualTo(formula.toString().length());
    }
}
