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
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.util.Pair;

import java.util.List;

/**
 * Tests for incremental cardinality constraints generated as formulas and {@link CCIncrementalData}.
 * @version 2.0.0
 * @since 1.1
 */
public class CCIncrementalFormulaTest implements LogicNGTest {

    private final FormulaFactory f = new FormulaFactory();
    private final SATSolver[] solvers;
    private final CCEncoder[] encoders;

    public CCIncrementalFormulaTest() {
        this.encoders = new CCEncoder[3];
        this.encoders[0] = new CCEncoder(this.f, CCConfig.builder().amkEncoding(CCConfig.AMK_ENCODER.TOTALIZER).alkEncoding(CCConfig.ALK_ENCODER.TOTALIZER).build());
        this.encoders[1] = new CCEncoder(this.f, CCConfig.builder().amkEncoding(CCConfig.AMK_ENCODER.CARDINALITY_NETWORK).alkEncoding(CCConfig.ALK_ENCODER.CARDINALITY_NETWORK).build());
        this.encoders[2] = new CCEncoder(this.f, CCConfig.builder().amkEncoding(CCConfig.AMK_ENCODER.MODULAR_TOTALIZER).alkEncoding(CCConfig.ALK_ENCODER.MODULAR_TOTALIZER).build());
        this.solvers = new SATSolver[4];
        this.solvers[0] = MiniSat.miniSat(this.f);
        this.solvers[1] = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(false).build());
        this.solvers[2] = MiniSat.miniCard(this.f);
        this.solvers[3] = MiniSat.glucose(this.f);
    }

    @Test
    public void testSimpleIncrementalAMK() {
        for (final CCEncoder encoder : this.encoders) {
            final CCEncoder initialEncoder = new CCEncoder(this.f);
            final int numLits = 10;
            final Variable[] vars = new Variable[numLits];
            for (int i = 0; i < numLits; i++) {
                vars[i] = this.f.variable("v" + i);
            }
            final Pair<List<Formula>, CCIncrementalData> cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.LE, 9, vars));
            final CCIncrementalData incData = cc.second();

            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(initialEncoder.encode((CardinalityConstraint) this.f.cc(CType.GE, 4, vars))); // >= 4
            solver.add(initialEncoder.encode((CardinalityConstraint) this.f.cc(CType.LE, 7, vars))); // <= 7

            solver.add(cc.first());
            assertSolverSat(solver);
            assertSolverSat(solver); // <= 9
            solver.add(incData.newUpperBound(8)); // <= 8
            assertSolverSat(solver);
            assertThat(incData.currentRHS()).isEqualTo(8);
            solver.add(incData.newUpperBound(7)); // <= 7
            assertSolverSat(solver);
            solver.add(incData.newUpperBound(6)); // <= 6
            assertSolverSat(solver);
            solver.add(incData.newUpperBound(5)); // <= 5
            assertSolverSat(solver);
            solver.add(incData.newUpperBound(4)); // <= 4
            assertSolverSat(solver);

            final SolverState state = solver.saveState();
            solver.add(incData.newUpperBound(3)); // <= 3
            assertSolverUnsat(solver);
            solver.loadState(state);
            assertSolverSat(solver);

            solver.add(incData.newUpperBound(2)); // <= 2
            assertSolverUnsat(solver);
        }
    }

    @Test
    public void testIncrementalData() {
        for (final CCEncoder encoder : this.encoders) {
            final int numLits = 10;
            final Variable[] vars = new Variable[numLits];
            for (int i = 0; i < numLits; i++) {
                vars[i] = this.f.variable("v" + i);
            }
            Pair<List<Formula>, CCIncrementalData> cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.LT, 10, vars));
            CCIncrementalData incData = cc.second();
            assertThat(incData.toString()).contains("currentRHS=9");

            cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.GT, 1, vars));
            incData = cc.second();
            assertThat(incData.toString()).contains("currentRHS=2");

            cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.LT, 1, vars));
            incData = cc.second();
            assertThat(incData).isNull();
            assertThat(cc.first()).contains(vars[0].negate());

            cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.LE, numLits + 1, vars));
            incData = cc.second();
            assertThat(incData).isNull();

            cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.GE, numLits + 1, vars));
            incData = cc.second();
            assertThat(incData).isNull();

            cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.GE, numLits, vars));
            incData = cc.second();
            assertThat(incData).isNull();

            cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.GE, 0, vars));
            incData = cc.second();
            assertThat(incData).isNull();

            cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.GE, 1, vars));
            incData = cc.second();
            assertThat(incData).isNull();
        }
    }

    @Test
    public void testSimpleIncrementalALK() {
        for (final CCEncoder encoder : this.encoders) {
            final CCEncoder initialEncoder = new CCEncoder(this.f);
            final int numLits = 10;
            final Variable[] vars = new Variable[numLits];
            for (int i = 0; i < numLits; i++) {
                vars[i] = this.f.variable("v" + i);
            }
            final Pair<List<Formula>, CCIncrementalData> cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.GE, 2, vars));
            final CCIncrementalData incData = cc.second();

            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(initialEncoder.encode((CardinalityConstraint) this.f.cc(CType.GE, 4, vars))); // >= 4
            solver.add(initialEncoder.encode((CardinalityConstraint) this.f.cc(CType.LE, 7, vars))); // <= 7

            solver.add(cc.first());
            assertSolverSat(solver); // >=2
            solver.add(incData.newLowerBound(3)); // >= 3
            assertSolverSat(solver);
            solver.add(incData.newLowerBound(4)); // >= 4
            assertSolverSat(solver);
            solver.add(incData.newLowerBound(5)); // >= 5
            assertSolverSat(solver);
            solver.add(incData.newLowerBound(6)); // >= 6
            assertSolverSat(solver);
            solver.add(incData.newLowerBound(7)); // >= 7
            assertSolverSat(solver);

            final SolverState state = solver.saveState();
            solver.add(incData.newLowerBound(8)); // >= 8
            assertSolverUnsat(solver);
            solver.loadState(state);
            assertSolverSat(solver);
            solver.add(incData.newLowerBound(9)); // <= 9
            assertSolverUnsat(solver);
        }
    }

    @Test
    public void testLargeTotalizerUpperBoundAMK() {
        final CCEncoder encoder = this.encoders[0];
        final CCEncoder initivalEncoder = new CCEncoder(this.f);
        final int numLits = 100;
        int currentBound = numLits - 1;
        final Variable[] vars = new Variable[numLits];
        for (int i = 0; i < numLits; i++) {
            vars[i] = this.f.variable("v" + i);
        }
        final Pair<List<Formula>, CCIncrementalData> cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.LE, currentBound, vars));
        final CCIncrementalData incData = cc.second();

        final SATSolver solver = this.solvers[3];
        solver.reset();
        solver.add(initivalEncoder.encode((CardinalityConstraint) this.f.cc(CType.GE, 42, vars))); // >= 42
        solver.add(cc.first());

        // search the lower bound
        while (solver.sat() == Tristate.TRUE) {
            solver.add(incData.newUpperBound(--currentBound)); // <= currentBound - 1
        }
        assertThat(currentBound).isEqualTo(41);
    }

    @Test
    public void testLargeTotalizerLowerBoundALK() {
        final CCEncoder encoder = this.encoders[0];
        final CCEncoder initivalEncoder = new CCEncoder(this.f);
        final int numLits = 100;
        int currentBound = 2;
        final Variable[] vars = new Variable[numLits];
        for (int i = 0; i < numLits; i++) {
            vars[i] = this.f.variable("v" + i);
        }
        final Pair<List<Formula>, CCIncrementalData> cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.GE, currentBound, vars));
        final CCIncrementalData incData = cc.second();

        final SATSolver solver = this.solvers[3];
        solver.reset();
        solver.add(initivalEncoder.encode((CardinalityConstraint) this.f.cc(CType.LE, 87, vars))); // <= 42
        solver.add(cc.first());

        // search the lower bound
        while (solver.sat() == Tristate.TRUE) {
            solver.add(incData.newLowerBound(++currentBound)); // <= currentBound + 1
        }
        assertThat(currentBound).isEqualTo(88);
    }

    @Test
    @LongRunningTag
    public void testLargeModularTotalizerAMK() {
        for (final SATSolver solver : this.solvers) {
            final CCEncoder encoder = this.encoders[2];
            final CCEncoder initialEncoder = new CCEncoder(this.f);
            final int numLits = 100;
            int currentBound = numLits - 1;
            final Variable[] vars = new Variable[numLits];
            for (int i = 0; i < numLits; i++) {
                vars[i] = this.f.variable("v" + i);
            }
            final Pair<List<Formula>, CCIncrementalData> cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.LE, currentBound, vars));
            final CCIncrementalData incData = cc.second();

            solver.reset();
            solver.add(initialEncoder.encode((CardinalityConstraint) this.f.cc(CType.GE, 42, vars))); // >= 42
            solver.add(cc.first());

            // search the lower bound
            while (solver.sat() == Tristate.TRUE) {
                solver.add(incData.newUpperBound(--currentBound)); // <= currentBound - 1
            }
            assertThat(currentBound).isEqualTo(41);
        }
    }

    @Test
    public void testToString() {
        final String expected = String.format("CCConfig{%n" +
                "amoEncoder=BEST%n" +
                "amkEncoder=TOTALIZER%n" +
                "alkEncoder=TOTALIZER%n" +
                "exkEncoder=BEST%n" +
                "bimanderGroupSize=SQRT%n" +
                "bimanderFixedGroupSize=3%n" +
                "nestingGroupSize=4%n" +
                "productRecursiveBound=20%n" +
                "commanderGroupSize=3%n" +
                "}%n");
        assertThat(this.encoders[0].config().toString()).isEqualTo(expected);
        assertThat(this.encoders[0].toString()).isEqualTo(expected);
    }

    @Test
    @LongRunningTag
    public void testVeryLargeModularTotalizerAMK() {
        final CCEncoder encoder = this.encoders[2];
        final CCEncoder initivalEncoder = new CCEncoder(this.f);
        final int numLits = 300;
        int currentBound = numLits - 1;
        final Variable[] vars = new Variable[numLits];
        for (int i = 0; i < numLits; i++) {
            vars[i] = this.f.variable("v" + i);
        }
        final Pair<List<Formula>, CCIncrementalData> cc = encoder.encodeIncremental((CardinalityConstraint) this.f.cc(CType.LE, currentBound, vars));
        final CCIncrementalData incData = cc.second();

        final SATSolver solver = this.solvers[3];
        solver.reset();
        solver.add(initivalEncoder.encode((CardinalityConstraint) this.f.cc(CType.GE, 234, vars)));
        solver.add(cc.first());

        // search the lower bound
        while (solver.sat() == Tristate.TRUE) {
            solver.add(incData.newUpperBound(--currentBound));
        }
        assertThat(currentBound).isEqualTo(233);
    }
}
