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

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.UNDEF;

import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Literal;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Consumer;

/**
 * A solver function which returns all unit propagated literals on level 0 of the current
 * formula on the solver.  If the formula is UNSAT, {@code null} will be returned.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class UpZeroLiteralsFunction implements SolverFunction<SortedSet<Literal>> {

    private final static UpZeroLiteralsFunction INSTANCE = new UpZeroLiteralsFunction();

    /**
     * Private empty constructor.  Singleton class.
     */
    private UpZeroLiteralsFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the function.
     * @return the function instance
     */
    public static UpZeroLiteralsFunction get() {
        return INSTANCE;
    }

    @Override
    public SortedSet<Literal> apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        if (solver.getResult() == UNDEF) {
            throw new IllegalStateException("Cannot get unit propagated literals on level 0 as long as the formula is not solved.  Call 'sat' first.");
        }
        if (solver.getResult() == FALSE) {
            return null;
        }
        final LNGIntVector literals = solver.underlyingSolver().upZeroLiterals();
        final SortedSet<Literal> upZeroLiterals = new TreeSet<>();
        for (int i = 0; i < literals.size(); ++i) {
            final int lit = literals.get(i);
            upZeroLiterals.add(solver.factory().literal(solver.underlyingSolver().nameForIdx(MiniSatStyleSolver.var(lit)), !MiniSatStyleSolver.sign(lit)));
        }
        return upZeroLiterals;
    }
}
