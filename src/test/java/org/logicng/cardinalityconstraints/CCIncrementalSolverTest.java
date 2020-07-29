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

package org.logicng.cardinalityconstraints;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.LogicNGTest;
import org.logicng.LongRunningTag;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.CardinalityConstraint;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.MiniSatConfig;

/**
 * Tests for incremental cardinality constraints generated on the solver and {@link CCIncrementalData}.
 * @version 2.0.0
 * @since 1.1
 */
public class CCIncrementalSolverTest implements LogicNGTest {

    private final FormulaFactory f = new FormulaFactory();
    private final SATSolver[] solvers;
    private final CCConfig[] configs;

    public CCIncrementalSolverTest() {
        this.configs = new CCConfig[3];
        this.configs[0] = CCConfig.builder().amkEncoding(CCConfig.AMK_ENCODER.TOTALIZER).alkEncoding(CCConfig.ALK_ENCODER.TOTALIZER).build();
        this.configs[1] = CCConfig.builder().amkEncoding(CCConfig.AMK_ENCODER.CARDINALITY_NETWORK).alkEncoding(CCConfig.ALK_ENCODER.CARDINALITY_NETWORK).build();
        this.configs[2] = CCConfig.builder().amkEncoding(CCConfig.AMK_ENCODER.MODULAR_TOTALIZER).alkEncoding(CCConfig.ALK_ENCODER.MODULAR_TOTALIZER).build();
        this.solvers = new SATSolver[4];
        this.solvers[0] = MiniSat.miniSat(this.f);
        this.solvers[1] = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(false).build());
        this.solvers[2] = MiniSat.miniCard(this.f);
        this.solvers[3] = MiniSat.glucose(this.f);
    }

    @Test
    public void testSimpleIncrementalAMK() {
        for (final CCConfig config : this.configs) {
            this.f.putConfiguration(this.configs[2]);
            final int numLits = 10;
            final Variable[] vars = new Variable[numLits];
            for (int i = 0; i < numLits; i++) {
                vars[i] = this.f.variable("v" + i);
            }
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(this.f.cc(CType.GE, 4, vars)); // >= 4
            solver.add(this.f.cc(CType.LE, 7, vars)); // <= 7

            this.f.putConfiguration(config);

            final CCIncrementalData incData = solver.addIncrementalCC((CardinalityConstraint) this.f.cc(CType.LE, 9, vars));
            assertSolverSat(solver); // <= 9
            incData.newUpperBoundForSolver(8); // <= 8
            assertSolverSat(solver);
            incData.newUpperBoundForSolver(7); // <= 7
            assertSolverSat(solver);
            incData.newUpperBoundForSolver(6); // <= 6
            assertSolverSat(solver);
            incData.newUpperBoundForSolver(5); // <= 5
            assertSolverSat(solver);
            incData.newUpperBoundForSolver(4); // <= 4
            assertSolverSat(solver);

            final SolverState state = solver.saveState();
            incData.newUpperBoundForSolver(3); // <= 3
            assertSolverUnsat(solver);
            solver.loadState(state);
            assertSolverSat(solver);

            incData.newUpperBoundForSolver(2); // <= 2
            assertSolverUnsat(solver);
        }
    }

    @Test
    public void testSimpleIncrementalALK() {
        for (final CCConfig config : this.configs) {
            this.f.putConfiguration(this.configs[2]);
            final int numLits = 10;
            final Variable[] vars = new Variable[numLits];
            for (int i = 0; i < numLits; i++) {
                vars[i] = this.f.variable("v" + i);
            }
            final SATSolver solver = this.solvers[2];
            solver.reset();
            solver.add(this.f.cc(CType.GE, 4, vars)); // >= 4
            solver.add(this.f.cc(CType.LE, 7, vars)); // <= 7

            this.f.putConfiguration(config);

            final CCIncrementalData incData = solver.addIncrementalCC((CardinalityConstraint) this.f.cc(CType.GE, 2, vars));
            assertSolverSat(solver); // >=2
            incData.newLowerBoundForSolver(3); // >= 3
            assertSolverSat(solver);
            incData.newLowerBoundForSolver(4); // >= 4
            assertSolverSat(solver);
            incData.newLowerBoundForSolver(5); // >= 5
            assertSolverSat(solver);
            incData.newLowerBoundForSolver(6); // >= 6
            assertSolverSat(solver);
            incData.newLowerBoundForSolver(7); // >= 7
            assertSolverSat(solver);

            final SolverState state = solver.saveState();
            incData.newLowerBoundForSolver(8); // >= 8
            assertSolverUnsat(solver);
            solver.loadState(state);
            assertSolverSat(solver);

            incData.newLowerBoundForSolver(9); // <= 9
            assertSolverUnsat(solver);
        }
    }

    @Test
    public void testLargeTotalizerUpperBoundAMK() {
        this.f.putConfiguration(this.configs[2]);
        final int numLits = 100;
        int currentBound = numLits - 1;
        final Variable[] vars = new Variable[numLits];
        for (int i = 0; i < numLits; i++) {
            vars[i] = this.f.variable("v" + i);
        }
        final SATSolver solver = this.solvers[3];
        solver.reset();
        solver.add(this.f.cc(CType.GE, 42, vars)); // >= 42
        this.f.putConfiguration(this.configs[0]);
        final CCIncrementalData incData = solver.addIncrementalCC((CardinalityConstraint) this.f.cc(CType.LE, currentBound, vars));
        // search the lower bound
        while (solver.sat() == Tristate.TRUE) {
            incData.newUpperBoundForSolver(--currentBound); // <= currentBound - 1
        }
        assertThat(currentBound).isEqualTo(41);
    }

    @Test
    public void testLargeTotalizerLowerBoundALK() {
        this.f.putConfiguration(this.configs[2]);
        final int numLits = 100;
        int currentBound = 2;
        final Variable[] vars = new Variable[numLits];
        for (int i = 0; i < numLits; i++) {
            vars[i] = this.f.variable("v" + i);
        }
        final SATSolver solver = this.solvers[0];
        solver.reset();
        solver.add(this.f.cc(CType.LE, 87, vars));
        this.f.putConfiguration(this.configs[0]);
        final CCIncrementalData incData = solver.addIncrementalCC((CardinalityConstraint) this.f.cc(CType.GE, currentBound, vars));
        // search the lower bound
        while (solver.sat() == Tristate.TRUE) {
            incData.newLowerBoundForSolver(++currentBound); // <= currentBound + 1
        }
        assertThat(currentBound).isEqualTo(88);
    }

    @Test
    @LongRunningTag
    public void testLargeModularTotalizerAMK() {
        for (final SATSolver solver : this.solvers) {
            if (solver != null) {
                this.f.putConfiguration(this.configs[2]);
                final int numLits = 100;
                int currentBound = numLits - 1;
                final Variable[] vars = new Variable[numLits];
                for (int i = 0; i < numLits; i++) {
                    vars[i] = this.f.variable("v" + i);
                }
                solver.reset();
                solver.add(this.f.cc(CType.GE, 42, vars)); // >= 42
                final CCIncrementalData incData = solver.addIncrementalCC((CardinalityConstraint) this.f.cc(CType.LE, currentBound, vars));
                // search the lower bound
                while (solver.sat() == Tristate.TRUE) {
                    incData.newUpperBoundForSolver(--currentBound); // <= currentBound - 1
                }
                assertThat(currentBound).isEqualTo(41);
            }
        }
    }

    @Test
    @LongRunningTag
    public void testVeryLargeModularTotalizerAMK() {
        this.f.putConfiguration(this.configs[2]);
        final int numLits = 300;
        int currentBound = numLits - 1;
        final Variable[] vars = new Variable[numLits];
        for (int i = 0; i < numLits; i++) {
            vars[i] = this.f.variable("v" + i);
        }
        final SATSolver solver = this.solvers[3];
        solver.reset();
        solver.add(this.f.cc(CType.GE, 234, vars));
        final CCIncrementalData incData = solver.addIncrementalCC((CardinalityConstraint) this.f.cc(CType.LE, currentBound, vars));
        // search the lower bound
        while (solver.sat() == Tristate.TRUE) {
            incData.newUpperBoundForSolver(--currentBound);
        }
        assertThat(currentBound).isEqualTo(233);
    }
}
