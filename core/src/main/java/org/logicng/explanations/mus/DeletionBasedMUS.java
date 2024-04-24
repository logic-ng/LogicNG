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

package org.logicng.explanations.mus;

import static org.logicng.handlers.Handler.aborted;
import static org.logicng.handlers.Handler.start;

import org.logicng.datastructures.Tristate;
import org.logicng.explanations.UNSATCore;
import org.logicng.formulas.FormulaFactory;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SolverState;

import java.util.ArrayList;
import java.util.List;

/**
 * A naive deletion-based MUS algorithm.
 * @version 2.1.0
 * @since 1.1
 */
public final class DeletionBasedMUS extends MUSAlgorithm {

    @Override
    public <T extends Proposition> UNSATCore<T> computeMUS(final List<T> propositions, final FormulaFactory f, final MUSConfig config) {
        start(config.handler);
        final List<T> mus = new ArrayList<>(propositions.size());
        final List<SolverState> solverStates = new ArrayList<>(propositions.size());
        final MiniSat solver = MiniSat.miniSat(f);
        for (final Proposition proposition : propositions) {
            solverStates.add(solver.saveState());
            solver.add(proposition);
        }
        boolean sat = solver.sat() == Tristate.TRUE;
        if (aborted(config.handler)) {
            return null;
        }
        if (sat) {
            throw new IllegalArgumentException("Cannot compute a MUS for a satisfiable formula set.");
        }
        for (int i = solverStates.size() - 1; i >= 0; i--) {
            solver.loadState(solverStates.get(i));
            for (final Proposition prop : mus) {
                solver.add(prop);
            }
            sat = solver.sat(config.handler) == Tristate.TRUE;
            if (aborted(config.handler)) {
                return null;
            }
            if (sat) {
                mus.add(propositions.get(i));
            }
        }
        return new UNSATCore<>(mus, true);
    }
}
