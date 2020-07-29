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
package org.logicng.pseudobooleans;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.LogicNGTest;
import org.logicng.cardinalityconstraints.CCConfig;
import org.logicng.configurations.ConfigurationType;
import org.logicng.formulas.CType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for {@link PBEncoder}.
 * @version 2.0.0
 * @since 1.0
 */
public class PBEncoderTest implements LogicNGTest {

    private final FormulaFactory f = new FormulaFactory();
    private final PBEncoder[] encoders;

    public PBEncoderTest() {
        this.encoders = new PBEncoder[3];
        this.encoders[0] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.SWC).build());
        this.encoders[1] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.BINARY_MERGE).binaryMergeUseGAC(false).build(), CCConfig.builder().amoEncoding(CCConfig.AMO_ENCODER.NESTED).build());
        this.encoders[2] = new PBEncoder(this.f, null);
    }

    @Test
    public void testCC0() {
        for (final PBEncoder encoder : this.encoders) {
            final int numLits = 100;
            final List<Literal> lits = new ArrayList<>();
            final List<Integer> coeffs = new ArrayList<>();
            final Variable[] problemLits = new Variable[numLits];
            for (int i = 0; i < numLits; i++) {
                final Variable var = this.f.variable("v" + i);
                lits.add(var);
                problemLits[i] = var;
                coeffs.add(1);
            }
            final List<Formula> clauses = encoder.encode((PBConstraint) this.f.pbc(CType.LE, 0, lits, coeffs));
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(clauses);
            assertSolverSat(solver);
            assertThat(solver.enumerateAllModels(problemLits))
                    .hasSize(1)
                    .allMatch(m -> m.positiveVariables().isEmpty());
        }
    }

    @Test
    public void testCC1() {
        for (final PBEncoder encoder : this.encoders) {
            final int numLits = 100;
            final int rhs = 1;
            final List<Literal> lits = new ArrayList<>();
            final List<Integer> coeffs = new ArrayList<>();
            final Variable[] problemLits = new Variable[numLits];
            for (int i = 0; i < numLits; i++) {
                final Variable var = this.f.variable("v" + i);
                lits.add(var);
                problemLits[i] = var;
                coeffs.add(1);
            }
            final List<Formula> clauses = encoder.encode((PBConstraint) this.f.pbc(CType.LE, rhs, lits, coeffs));
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(clauses);
            assertSolverSat(solver);
            assertThat(solver.enumerateAllModels(problemLits))
                    .hasSize(numLits + 1)
                    .allMatch(m -> m.positiveVariables().size() <= rhs);
        }
    }

    @Test
    public void testCCs() {
        for (final PBEncoder encoder : this.encoders) {
            testCC(10, 0, 1, encoder);
            testCC(10, 1, 11, encoder);
            testCC(10, 2, 56, encoder);
            testCC(10, 3, 176, encoder);
            testCC(10, 4, 386, encoder);
            testCC(10, 5, 638, encoder);
            testCC(10, 6, 848, encoder);
            testCC(10, 7, 968, encoder);
            testCC(10, 8, 1013, encoder);
            testCC(10, 9, 1023, encoder);
        }
    }

    private void testCC(final int numLits, final int rhs, final int expected, final PBEncoder encoder) {
        final Variable[] problemLits = new Variable[numLits];
        final int[] coeffs = new int[numLits];
        for (int i = 0; i < numLits; i++) {
            problemLits[i] = this.f.variable("v" + i);
            coeffs[i] = 1;
        }
        final List<Formula> clauses = encoder.encode((PBConstraint) this.f.pbc(CType.LE, rhs, problemLits, coeffs));
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(clauses);
        assertSolverSat(solver);
        assertThat(solver.enumerateAllModels(problemLits))
                .hasSize(expected)
                .allMatch(m -> m.positiveVariables().size() <= rhs);
    }

    @Test
    public void testNotNullConfig() {
        Arrays.stream(this.encoders).forEach(e -> assertThat(e.config()).isNotNull());
    }

    @Test
    public void testSpecialCases() {
        final List<Literal> lits = new ArrayList<>();
        lits.add(this.f.literal("m", true));
        lits.add(this.f.literal("n", true));
        final List<Integer> coeffs = new ArrayList<>();
        coeffs.add(2);
        coeffs.add(1);
        final PBConstraint truePBC = (PBConstraint) this.f.pbc(CType.GE, 0, lits, coeffs);
        for (final PBEncoder encoder : this.encoders) {
            assertThat(encoder.encode(truePBC)).isEmpty();
        }
    }

    @Test
    public void testCCNormalized() throws ParserException {
        final List<Literal> lits = new ArrayList<>();
        lits.add(this.f.literal("m", true));
        lits.add(this.f.literal("n", true));
        final List<Integer> coeffs2 = new ArrayList<>();
        coeffs2.add(2);
        coeffs2.add(2);
        final PBConstraint normCC = (PBConstraint) this.f.pbc(CType.LE, 2, lits, coeffs2);
        assertThat(this.encoders[0].encode(normCC)).containsExactly(this.f.parse("~m | ~n"));
    }

    @Test
    public void testConfigToString() {
        assertThat(this.encoders[0].config().toString()).isEqualTo(String.format("PBConfig{%n" +
                "pbEncoder=SWC%n" +
                "binaryMergeUseGAC=true%n" +
                "binaryMergeNoSupportForSingleBit=false%n" +
                "binaryMergeUseWatchDog=true%n" +
                "}%n"));
        assertThat(this.encoders[0].config().toString().contains("pbEncoder=" + PBConfig.PB_ENCODER.valueOf("SWC"))).isTrue();
        assertThat(this.encoders[0].config().type()).isEqualTo(ConfigurationType.PB_ENCODER);
        assertThat(new PBSWC(this.f).toString()).isEqualTo("PBSWC");
        assertThat(Arrays.asList(PBConfig.PB_ENCODER.values()).contains(PBConfig.PB_ENCODER.valueOf("ADDER_NETWORKS"))).isTrue();
    }
}
