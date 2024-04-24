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
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.CType;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.sat.MiniSatConfig;

import java.util.List;

/**
 * Test the solving (via encoding) of pseudo-Boolean constraints.
 * @version 2.0.0
 * @since 1.0
 */
public class PBSolvingTest implements LogicNGTest {

    private final FormulaFactory f;
    private final Variable[] literals100;
    private final Variable[] literals10;
    private final SATSolver[] solvers;

    private final PBEncoder[] encoders;

    public PBSolvingTest() {
        this.f = new FormulaFactory();
        this.literals100 = new Variable[100];
        this.literals10 = new Variable[10];
        for (int i = 0; i < 100; i++) {
            this.literals100[i] = this.f.variable("v" + i);
        }
        for (int i = 0; i < 10; i++) {
            this.literals10[i] = this.f.variable("v" + i);
        }
        this.solvers = new SATSolver[4];
        this.solvers[0] = MiniSat.miniSat(this.f);
        this.solvers[1] = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(false).build());
        this.solvers[2] = MiniSat.miniCard(this.f);
        this.solvers[3] = MiniSat.glucose(this.f);
        this.encoders = new PBEncoder[10];
        this.encoders[0] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.SWC).build());
        this.encoders[1] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.BINARY_MERGE).binaryMergeUseGAC(true).binaryMergeNoSupportForSingleBit(true).binaryMergeUseWatchDog(true).build());
        this.encoders[2] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.BINARY_MERGE).binaryMergeUseGAC(true).binaryMergeNoSupportForSingleBit(true).binaryMergeUseWatchDog(false).build());
        this.encoders[3] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.BINARY_MERGE).binaryMergeUseGAC(true).binaryMergeNoSupportForSingleBit(false).binaryMergeUseWatchDog(true).build());
        this.encoders[4] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.BINARY_MERGE).binaryMergeUseGAC(true).binaryMergeNoSupportForSingleBit(false).binaryMergeUseWatchDog(false).build());
        this.encoders[5] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.BINARY_MERGE).binaryMergeUseGAC(false).binaryMergeNoSupportForSingleBit(true).binaryMergeUseWatchDog(true).build());
        this.encoders[6] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.BINARY_MERGE).binaryMergeUseGAC(false).binaryMergeNoSupportForSingleBit(true).binaryMergeUseWatchDog(false).build());
        this.encoders[7] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.BINARY_MERGE).binaryMergeUseGAC(false).binaryMergeNoSupportForSingleBit(false).binaryMergeUseWatchDog(true).build());
        this.encoders[8] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.BINARY_MERGE).binaryMergeUseGAC(false).binaryMergeNoSupportForSingleBit(false).binaryMergeUseWatchDog(false).build());
        this.encoders[9] = new PBEncoder(this.f, PBConfig.builder().pbEncoding(PBConfig.PB_ENCODER.ADDER_NETWORKS).build());
    }

    @Test
    public void testCCAMO() {
        for (final SATSolver solver : this.solvers) {
            solver.reset();
            solver.add(this.f.amo(this.literals100));
            final List<Assignment> models = solver.enumerateAllModels(this.literals100);
            assertThat(models.size()).isEqualTo(101);
            for (final Assignment model : models) {
                assertThat(model.positiveVariables().size() <= 1).isTrue();
            }
        }
    }

    @Test
    public void testCCEXO() {
        for (final SATSolver solver : this.solvers) {
            solver.reset();
            solver.add(this.f.exo(this.literals100));
            final List<Assignment> models = solver.enumerateAllModels(this.literals100);
            assertThat(models.size()).isEqualTo(100);
            for (final Assignment model : models) {
                assertThat(model.positiveVariables().size() == 1).isTrue();
            }
        }
    }

    @Test
    public void testCCAMK() {
        for (final SATSolver solver : this.solvers) {
            testCCAMK(solver, 0, 1);
            testCCAMK(solver, 1, 11);
            testCCAMK(solver, 2, 56);
            testCCAMK(solver, 3, 176);
            testCCAMK(solver, 4, 386);
            testCCAMK(solver, 5, 638);
            testCCAMK(solver, 6, 848);
            testCCAMK(solver, 7, 968);
            testCCAMK(solver, 8, 1013);
            testCCAMK(solver, 9, 1023);
        }
    }

    @Test
    public void testCCLT() {
        for (final SATSolver solver : this.solvers) {
            testCCLT(solver, 1, 1);
            testCCLT(solver, 2, 11);
            testCCLT(solver, 3, 56);
            testCCLT(solver, 4, 176);
            testCCLT(solver, 5, 386);
            testCCLT(solver, 6, 638);
            testCCLT(solver, 7, 848);
            testCCLT(solver, 8, 968);
            testCCLT(solver, 9, 1013);
            testCCLT(solver, 10, 1023);
        }
    }

    @Test
    public void testCCALK() {
        for (final SATSolver solver : this.solvers) {
            testCCALK(solver, 1, 1023);
            testCCALK(solver, 2, 1013);
            testCCALK(solver, 3, 968);
            testCCALK(solver, 4, 848);
            testCCALK(solver, 5, 638);
            testCCALK(solver, 6, 386);
            testCCALK(solver, 7, 176);
            testCCALK(solver, 8, 56);
            testCCALK(solver, 9, 11);
            testCCALK(solver, 10, 1);
        }
    }

    @Test
    public void testCCGT() {
        for (final SATSolver solver : this.solvers) {
            testCCGT(solver, 0, 1023);
            testCCGT(solver, 1, 1013);
            testCCGT(solver, 2, 968);
            testCCGT(solver, 3, 848);
            testCCGT(solver, 4, 638);
            testCCGT(solver, 5, 386);
            testCCGT(solver, 6, 176);
            testCCGT(solver, 7, 56);
            testCCGT(solver, 8, 11);
            testCCGT(solver, 9, 1);
        }
    }

    @Test
    public void testCCEQ() {
        for (final SATSolver solver : this.solvers) {
            testCCEQ(solver, 0, 1);
            testCCEQ(solver, 1, 10);
            testCCEQ(solver, 2, 45);
            testCCEQ(solver, 3, 120);
            testCCEQ(solver, 4, 210);
            testCCEQ(solver, 5, 252);
            testCCEQ(solver, 6, 210);
            testCCEQ(solver, 7, 120);
            testCCEQ(solver, 8, 45);
            testCCEQ(solver, 9, 10);
            testCCEQ(solver, 10, 1);
        }
    }

    private void testCCAMK(final SATSolver solver, final int rhs, final int expected) {
        solver.reset();
        solver.add(this.f.cc(CType.LE, rhs, this.literals10));
        assertSolverSat(solver);
        assertThat(solver.enumerateAllModels(this.literals10))
                .hasSize(expected)
                .allMatch(model -> model.positiveVariables().size() <= rhs);
    }

    private void testCCLT(final SATSolver solver, final int rhs, final int expected) {
        solver.reset();
        solver.add(this.f.cc(CType.LT, rhs, this.literals10));
        assertSolverSat(solver);
        assertThat(solver.enumerateAllModels(this.literals10))
                .hasSize(expected)
                .allMatch(model -> model.positiveVariables().size() < rhs);
    }

    private void testCCALK(final SATSolver solver, final int rhs, final int expected) {
        solver.reset();
        solver.add(this.f.cc(CType.GE, rhs, this.literals10));
        assertSolverSat(solver);
        assertThat(solver.enumerateAllModels(this.literals10))
                .hasSize(expected)
                .allMatch(model -> model.positiveVariables().size() >= rhs);
    }

    private void testCCGT(final SATSolver solver, final int rhs, final int expected) {
        solver.reset();
        solver.add(this.f.cc(CType.GT, rhs, this.literals10));
        assertSolverSat(solver);
        assertThat(solver.enumerateAllModels(this.literals10))
                .hasSize(expected)
                .allMatch(model -> model.positiveVariables().size() > rhs);
    }

    private void testCCEQ(final SATSolver solver, final int rhs, final int expected) {
        solver.reset();
        solver.add(this.f.cc(CType.EQ, rhs, this.literals10));
        assertSolverSat(solver);
        assertThat(solver.enumerateAllModels(this.literals10))
                .hasSize(expected)
                .allMatch(model -> model.positiveVariables().size() == rhs);
    }

    @Test
    public void testPBEQ() {
        for (final PBEncoder encoder : this.encoders) {
            for (final SATSolver solver : this.solvers) {
                solver.reset();
                final int[] coeffs10 = new int[]{3, 2, 2, 2, 2, 2, 2, 2, 2, 2};
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.EQ, 5, this.literals10, coeffs10)));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10))
                        .hasSize(9)
                        .allMatch(model -> model.positiveVariables().size() == 2)
                        .allMatch(model -> model.positiveVariables().contains(this.f.variable("v" + 0)));
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.EQ, 7, this.literals10, coeffs10)));
                assertSolverSat(solver);

                assertThat(solver.enumerateAllModels(this.literals10))
                        .hasSize(36)
                        .allMatch(model -> model.positiveVariables().size() == 3)
                        .allMatch(model -> model.positiveVariables().contains(this.f.variable("v" + 0)));
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.EQ, 0, this.literals10, coeffs10)));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(1);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.EQ, 1, this.literals10, coeffs10)));
                assertSolverUnsat(solver);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.EQ, 22, this.literals10, coeffs10)));
                assertSolverUnsat(solver);
            }
        }
    }

    @Test
    public void testPBLess() {
        for (final PBEncoder encoder : this.encoders) {
            for (final SATSolver solver : this.solvers) {
                solver.reset();
                final int[] coeffs10 = new int[]{3, 2, 2, 2, 2, 2, 2, 2, 2, 2};
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.LE, 6, this.literals10, coeffs10)));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10))
                        .hasSize(140)
                        .allMatch(model -> model.positiveVariables().size() <= 3);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.LT, 7, this.literals10, coeffs10)));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10))
                        .hasSize(140)
                        .allMatch(model -> model.positiveVariables().size() <= 3);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.LE, 0, this.literals10, coeffs10)));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(1);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.LE, 1, this.literals10, coeffs10)));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(1);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.LT, 2, this.literals10, coeffs10)));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(1);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.LT, 1, this.literals10, coeffs10)));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(1);
            }
        }
    }

    @Test
    public void testPBGreater() {
        for (final PBEncoder encoder : this.encoders) {
            for (final SATSolver solver : this.solvers) {
                solver.reset();
                final int[] coeffs10 = new int[]{3, 2, 2, 2, 2, 2, 2, 2, 2, 2};
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.GE, 17, this.literals10, coeffs10)));
                assertSolverSat(solver);

                assertThat(solver.enumerateAllModels(this.literals10))
                        .hasSize(47)
                        .allMatch(model -> model.positiveVariables().size() >= 8);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.GT, 16, this.literals10, coeffs10)));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10))
                        .hasSize(47)
                        .allMatch(model -> model.positiveVariables().size() >= 8);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.GE, 21, this.literals10, coeffs10)));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(1);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.GE, 22, this.literals10, coeffs10)));
                assertSolverUnsat(solver);
                solver.reset();
                solver.add(encoder.encode((PBConstraint) this.f.pbc(CType.GT, 42, this.literals10, coeffs10)));
                assertSolverUnsat(solver);
            }
        }
    }

    @Test
    public void testPBNegative() {
        for (final PBEncoder encoder : this.encoders) {
            for (final SATSolver solver : this.solvers) {
                solver.reset();
                int[] coeffs10 = new int[]{2, 2, 2, 2, 2, 2, 2, 2, 2, -2};
                final PBConstraint pbc = (PBConstraint) this.f.pbc(CType.EQ, 2, this.literals10, coeffs10);
                solver.add(encoder.encode(pbc));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(45).allMatch(pbc::evaluate);
                solver.reset();

                coeffs10 = new int[]{2, 2, 2, 2, 2, 2, 2, 2, 2, -2};
                final PBConstraint pbc2 = (PBConstraint) this.f.pbc(CType.EQ, 4, this.literals10, coeffs10);
                solver.add(encoder.encode(pbc2));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(120).allMatch(pbc2::evaluate);
                solver.reset();

                coeffs10 = new int[]{2, 2, -3, 2, -7, 2, 2, 2, 2, -2};
                final PBConstraint pbc3 = (PBConstraint) this.f.pbc(CType.EQ, 4, this.literals10, coeffs10);
                solver.add(encoder.encode(pbc3));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(57).allMatch(pbc3::evaluate);
                solver.reset();

                coeffs10 = new int[]{2, 2, -3, 2, -7, 2, 2, 2, 2, -2};
                final PBConstraint pbc4 = (PBConstraint) this.f.pbc(CType.EQ, -10, this.literals10, coeffs10);
                solver.add(encoder.encode(pbc4));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(8).allMatch(pbc4::evaluate);
                solver.reset();

                coeffs10 = new int[]{2, 2, -4, 2, -6, 2, 2, 2, 2, -2};
                final PBConstraint pbc5 = (PBConstraint) this.f.pbc(CType.EQ, -12, this.literals10, coeffs10);
                solver.add(encoder.encode(pbc5));
                assertSolverSat(solver);
                assertThat(solver.enumerateAllModels(this.literals10)).hasSize(1).allMatch(pbc5::evaluate);
                solver.reset();
            }
        }
    }

    @Test
    public void testLargePBs() {
        for (final PBEncoder encoder : this.encoders) {
            final SATSolver solver = this.solvers[0];
            solver.reset();
            final int numLits = 100;
            final Variable[] lits = new Variable[numLits];
            final int[] coeffs = new int[numLits];
            for (int i = 0; i < numLits; i++) {
                lits[i] = this.f.variable("v" + i);
                coeffs[i] = i + 1;
            }
            final PBConstraint pbc = (PBConstraint) this.f.pbc(CType.GE, 5000, lits, coeffs);
            solver.add(encoder.encode(pbc));
            assertSolverSat(solver);
            assertThat(pbc.evaluate(solver.model())).isTrue();
        }
    }
}
