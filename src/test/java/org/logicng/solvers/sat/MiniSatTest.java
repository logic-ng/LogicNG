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
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;

import org.junit.jupiter.api.Test;
import org.logicng.collections.LNGIntVector;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.Arrays;

/**
 * Some MiniSat specific unit tests.
 * @version 2.0.0
 * @since 1.0
 */
public class MiniSatTest {

    @Test
    public void testAnalyzeFinal() {
        final MiniSat2Solver solver = new MiniSat2Solver();
        solver.newVar(true, true);
        solver.newVar(true, true);
        solver.newVar(true, true);
        solver.newVar(true, true);
        solver.addClause(clause(1, 2, 3), null);
        solver.addClause(clause(-1, -2), null);
        solver.addClause(clause(-1, -3), null);
        solver.addClause(clause(-2, -3), null);
        assertThat(solver.solve(null)).isEqualTo(TRUE);
        assertThat(solver.solve(null, clause(1, 2))).isEqualTo(FALSE);
    }

    @Test
    public void testInvalidSaveState() {
        final MiniSat2Solver solver = new MiniSat2Solver(MiniSatConfig.builder().incremental(false).build());
        assertThatThrownBy(solver::saveState).isInstanceOf(IllegalStateException.class);
    }

    @Test
    public void testInvalidLoadState() {
        final MiniSat2Solver solver = new MiniSat2Solver(MiniSatConfig.builder().incremental(false).build());
        assertThatThrownBy(() -> solver.loadState(null)).isInstanceOf(IllegalStateException.class);
    }

    @Test
    public void testConfig() {
        assertThat(MiniSatConfig.builder().build().type().toString()).isEqualTo("MINISAT");
        assertThat(Arrays.asList(MiniSatConfig.ClauseMinimization.values()).contains(MiniSatConfig.ClauseMinimization.valueOf("DEEP"))).isTrue();
    }

    @Test
    public void testAssumptionChecking() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final SATSolver solver = MiniSat.miniSat(f);
        solver.add(f.parse("A & B"));
        assertThat(solver.sat()).isEqualTo(TRUE);
        assertThat(solver.sat(f.literal("A", true))).isEqualTo(TRUE);
        assertThat(solver.sat(f.literal("B", true))).isEqualTo(TRUE);
        assertThat(solver.sat(f.literal("A", false))).isEqualTo(FALSE);
        assertThat(solver.sat(f.literal("B", false))).isEqualTo(FALSE);
        assertThat(solver.sat(f.literal("A", true))).isEqualTo(TRUE);
        assertThat(solver.sat(f.literal("B", true))).isEqualTo(TRUE);
        assertThat(solver.sat(f.literal("A", false))).isEqualTo(FALSE);
        assertThat(solver.sat()).isEqualTo(TRUE);
    }

    private LNGIntVector clause(final int... lits) {
        final LNGIntVector c = new LNGIntVector(lits.length);
        for (final int l : lits) {
            c.push(literal(l));
        }
        return c;
    }

    private int literal(final int l) {
        return l < 0 ? (-l * 2) ^ 1 : l * 2;
    }
}
