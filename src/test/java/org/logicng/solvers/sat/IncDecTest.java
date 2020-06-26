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

package org.logicng.solvers.sat;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.LogicNGTest;
import org.logicng.formulas.FormulaFactory;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.testutils.PigeonHoleGenerator;

/**
 * Tests for the incremental/decremental interface of the SAT solvers.
 * @version 2.0.0
 * @since 1.0
 */
public class IncDecTest implements LogicNGTest {

    private final FormulaFactory f;
    private final MiniSat[] solvers;
    private final PigeonHoleGenerator pg;

    public IncDecTest() {
        this.f = new FormulaFactory();
        this.pg = new PigeonHoleGenerator(this.f);
        this.solvers = new MiniSat[2];
        this.solvers[0] = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(true).build());
        this.solvers[1] = MiniSat.miniCard(this.f, MiniSatConfig.builder().incremental(true).build());
    }

    @Test
    public void testIncDec() {
        for (final MiniSat s : this.solvers) {
            s.add(this.f.variable("a"));
            final SolverState state1 = s.saveState();
            if (s.underlyingSolver() instanceof MiniCard) {
                assertThat(state1.toString()).isEqualTo("SolverState{id=0, state=[1, 1, 0, 0, 1]}");
            } else {
                assertThat(state1.toString()).isEqualTo("SolverState{id=0, state=[1, 1, 0, 0, 1, 0, 0]}");
            }
            assertSolverSat(s);
            s.add(this.pg.generate(5));
            assertSolverUnsat(s);
            s.loadState(state1);
            assertSolverSat(s);
            s.add(this.f.literal("a", false));
            assertSolverUnsat(s);
            s.loadState(state1);
            assertSolverSat(s);
            s.add(this.pg.generate(5));
            final SolverState state2 = s.saveState();
            if (s.underlyingSolver() instanceof MiniCard) {
                assertThat(state2.toString()).isEqualTo("SolverState{id=1, state=[1, 31, 81, 0, 1]}");
            } else {
                assertThat(state2.toString()).isEqualTo("SolverState{id=1, state=[1, 31, 81, 0, 1, 0, 0]}");
            }
            s.add(this.pg.generate(4));
            assertSolverUnsat(s);
            s.loadState(state2);
            assertSolverUnsat(s);
            s.loadState(state1);
            assertSolverSat(s);
        }
    }

    @Test
    public void testIncDecDeep() {
        for (final SATSolver s : this.solvers) {
            s.add(this.f.variable("a"));
            final SolverState state1 = s.saveState();
            s.add(this.f.variable("b"));
            assertSolverSat(s);
            final SolverState state2 = s.saveState();
            s.add(this.f.literal("a", false));
            assertSolverUnsat(s);
            s.loadState(state1);
            try {
                s.loadState(state2);
                assert false;
            } catch (final IllegalArgumentException e) {
                // fine
            }
            s.add(this.f.literal("b", false));
            assertSolverSat(s);
            final SolverState state3 = s.saveState();
            s.add(this.f.literal("a", false));
            assertSolverUnsat(s);
            s.loadState(state3);
            s.add(this.f.variable("c"));
            final SolverState state4 = s.saveState();
            final SolverState state5 = s.saveState();
            s.loadState(state4);
            try {
                s.loadState(state5);
                assert false;
            } catch (final IllegalArgumentException e) {
                // fine
            }
            assertSolverSat(s);
            s.loadState(state1);
            assertSolverSat(s);
            try {
                s.loadState(state3);
                assert false;
            } catch (final IllegalArgumentException e) {
                // fine
            }
        }
    }
}
