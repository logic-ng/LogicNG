///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

package org.logicng.transformations;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.RandomTag;
import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.F;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;
import org.logicng.io.parsers.ParserException;
import org.logicng.modelcounting.ModelCounter;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.util.Collections;

/**
 * Unit Tests for {@link PureExpansionTransformation}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class PureExpansionTransformationTest {

    private static final FormulaFactory f = F.f;
    private static final PureExpansionTransformation transformation = new PureExpansionTransformation();

    @Test
    public void testConstants() {
        computeAndVerify(F.FALSE, transformation);
        computeAndVerify(F.TRUE, transformation);
    }

    @Test
    public void testLiterals() {
        assertThat(F.A.transform(transformation)).isEqualTo(F.A);
        assertThat(F.NA.transform(transformation)).isEqualTo(F.NA);
    }

    @Test
    public void testNot() throws ParserException {
        assertThat(F.NOT1.transform(transformation)).isEqualTo(F.NOT1);
        assertThat(F.NOT2.transform(transformation)).isEqualTo(F.NOT2);

        assertThat(f.parse("~a").transform(transformation)).isEqualTo(f.parse("~a"));
        assertThat(f.parse("~(a => b)").transform(transformation)).isEqualTo(f.parse("~(a => b)"));
        assertThat(f.parse("~(~(a | b) => ~(x | y))").transform(transformation)).isEqualTo(f.parse("~(~(a | b) => ~(x | y))"));
        assertThat(f.parse("~(a <=> b)").transform(transformation)).isEqualTo(f.parse("~(a <=> b)"));
        assertThat(f.parse("~(a & b & ~x & ~y)").transform(transformation)).isEqualTo(f.parse("~(a & b & ~x & ~y)"));
        assertThat(f.parse("~(a | b | (a + b <= 1) | ~y)").transform(transformation)).isEqualTo(f.parse("~(a | b | (~a | ~b) | ~y)"));
    }

    @Test
    public void testBinaryOperators() throws ParserException {
        assertThat(F.IMP1.transform(transformation)).isEqualTo(F.IMP1);
        assertThat(F.IMP2.transform(transformation)).isEqualTo(F.IMP2);
        assertThat(F.IMP3.transform(transformation)).isEqualTo(F.IMP3);
        assertThat(F.IMP4.transform(transformation)).isEqualTo(F.IMP4);
        assertThat(F.EQ1.transform(transformation)).isEqualTo(F.EQ1);
        assertThat(F.EQ2.transform(transformation)).isEqualTo(F.EQ2);
        assertThat(F.EQ3.transform(transformation)).isEqualTo(F.EQ3);
        assertThat(F.EQ4.transform(transformation)).isEqualTo(F.EQ4);

        assertThat(f.parse("~(a => (a + b = 1))").transform(transformation)).isEqualTo(f.parse("~(a => (a | b) & (~a | ~b))"));
    }

    @Test
    public void testNAryOperators() throws ParserException {
        assertThat(F.AND1.transform(transformation)).isEqualTo(F.AND1);
        assertThat(F.AND2.transform(transformation)).isEqualTo(F.AND2);
        assertThat(F.AND3.transform(transformation)).isEqualTo(F.AND3);
        assertThat(F.OR1.transform(transformation)).isEqualTo(F.OR1);
        assertThat(F.OR2.transform(transformation)).isEqualTo(F.OR2);
        assertThat(F.OR3.transform(transformation)).isEqualTo(F.OR3);

        assertThat(f.parse("~(a & b) | c | ~(x | ~y)").transform(transformation)).isEqualTo(f.parse("~(a & b) | c | ~(x | ~y)"));
        assertThat(f.parse("~(a | b) & (a + b = 1) & ~(x & ~(z + x = 1))").transform(transformation))
                .isEqualTo(f.parse("~(a | b) & ((a | b) & (~a | ~b)) & ~(x & ~((z | x) & (~z | ~x)))"));
        assertThat(f.parse("a & b & (~x | ~y)").transform(transformation)).isEqualTo(f.parse("a & b & (~x | ~y)"));
        assertThat(f.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(transformation)).isEqualTo(f.parse("~(a | b) & c & ~(x & ~y) & (w => z)"));
        assertThat(f.parse("~(a & b) | c | ~(x | ~y)").transform(transformation)).isEqualTo(f.parse("~(a & b) | c | ~(x | ~y)"));
        assertThat(f.parse("a | b | (~x & ~y)").transform(transformation)).isEqualTo(f.parse("a | b | (~x & ~y)"));
    }

    @Test
    public void testPBCs() throws ParserException {
        assertThat(f.parse("a + b <= 1").transform(transformation)).isEqualTo(f.parse("~a | ~b"));
        assertThat(f.parse("a + b < 2").transform(transformation)).isEqualTo(f.parse("~a | ~b"));
        assertThat(f.parse("a + b = 1").transform(transformation)).isEqualTo(f.parse("(a | b) & (~a | ~b)"));
    }

    @Test
    public void testExceptionalBehavior() {
        assertThatThrownBy(() -> F.PBC1.transform(transformation))
                .isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> F.PBC2.transform(transformation))
                .isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> F.PBC3.transform(transformation))
                .isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> F.PBC4.transform(transformation))
                .isInstanceOf(UnsupportedOperationException.class);
        assertThatThrownBy(() -> F.PBC5.transform(transformation))
                .isInstanceOf(UnsupportedOperationException.class);
    }

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        for (final Formula formula : cornerCases.cornerCases()) {
            if (formula.type() == FType.PBC) {
                final PBConstraint pbc = (PBConstraint) formula;
                if (!pbc.isAmo() && !pbc.isExo()) {
                    assertThatThrownBy(() -> ModelCounter.count(Collections.singletonList(formula), formula.variables()))
                            .isInstanceOf(UnsupportedOperationException.class);
                    continue;
                }
            }
            computeAndVerify(formula, transformation);
        }
    }

    @Test
    @RandomTag
    public void testRandom() {
        for (int i = 0; i < 200; i++) {
            final FormulaFactory f = new FormulaFactory();
            final FormulaRandomizerConfig config = FormulaRandomizerConfig.builder()
                    .numVars(12).weightAmo(5).weightExo(5).seed(42).build();
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, config);
            final Formula formula = randomizer.formula(5);
            computeAndVerify(formula, transformation);
        }
    }

    private static void computeAndVerify(final Formula formula, final PureExpansionTransformation transformation) {
        final Formula expandedFormula = formula.transform(transformation);
        verify(formula, expandedFormula);
    }

    private static void verify(final Formula formula, final Formula expandedFormula) {
        final FormulaFactory f = formula.factory();
        assertThat(f.equivalence(formula, expandedFormula).holds(new TautologyPredicate(f)));
        assertThat(isFreeOfPBCs(expandedFormula)).isTrue();
    }

    private static boolean isFreeOfPBCs(final Formula formula) {
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                return true;
            case NOT:
                return isFreeOfPBCs(((Not) formula).operand());
            case OR:
            case AND:
                for (final Formula op : formula) {
                    if (!isFreeOfPBCs(op)) {
                        return false;
                    }
                }
                return true;
            case IMPL:
            case EQUIV:
                final BinaryOperator binary = (BinaryOperator) formula;
                return isFreeOfPBCs(binary.left()) && isFreeOfPBCs(binary.right());
            case PBC:
                return false;
            default:
                throw new IllegalStateException("Unknown formula type: " + formula.type());
        }
    }

    @Test
    public void testToString() {
        assertThat(transformation.toString()).isEqualTo("PureExpansionTransformation");
    }
}
