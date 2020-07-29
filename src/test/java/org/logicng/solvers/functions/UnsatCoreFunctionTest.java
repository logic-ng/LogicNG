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

package org.logicng.solvers.functions;

import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.sat.MiniSatConfig;

/**
 * Unit tests for {@link UnsatCoreFunction}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class UnsatCoreFunctionTest extends TestWithExampleFormulas {

    @Test
    public void testExceptionalBehavior() {
        assertThatThrownBy(() -> {
            final SATSolver solver = MiniSat.miniSat(this.f, MiniSatConfig.builder().proofGeneration(false).build());
            solver.execute(UnsatCoreFunction.get());
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot generate an unsat core if proof generation is not turned on");
        assertThatThrownBy(() -> {
            final SATSolver solver = MiniSat.miniSat(this.f, MiniSatConfig.builder().proofGeneration(true).build());
            solver.sat();
            solver.execute(UnsatCoreFunction.get());
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("An unsat core can only be generated if the formula is solved and is UNSAT");
        assertThatThrownBy(() -> {
            final SATSolver solver = MiniSat.miniSat(this.f, MiniSatConfig.builder().proofGeneration(true).build());
            solver.execute(UnsatCoreFunction.get());
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot generate an unsat core before the formula was solved.");
        assertThatThrownBy(() -> {
            final SATSolver solver = MiniSat.miniCard(this.f, MiniSatConfig.builder().proofGeneration(true).build());
            solver.add(this.f.parse("A & (A => B) & (B => ~A)"));
            solver.sat();
            solver.execute(UnsatCoreFunction.get());
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot compute an unsat core with MiniCard.");
        assertThatThrownBy(() -> {
            final SATSolver solver = MiniSat.miniSat(this.f, MiniSatConfig.builder().proofGeneration(true).build());
            solver.add(this.f.variable("A"));
            solver.sat(this.f.literal("A", false));
            solver.execute(UnsatCoreFunction.get());
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot compute an unsat core for a computation with assumptions.");
    }
}
