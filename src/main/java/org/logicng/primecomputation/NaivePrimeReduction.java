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

package org.logicng.primecomputation;

import static org.logicng.handlers.Handler.aborted;
import static org.logicng.handlers.Handler.start;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.util.FormulaHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Naive implementation for reducing implicants and implicates to prime ones.
 * <p>
 * The computation is initialized with the formula for which
 * the prime implicants/implicates should be computed.
 * @version 2.1.0
 * @since 2.0.0
 */
public final class NaivePrimeReduction {

    private final SATSolver implicantSolver;
    private final SATSolver implicateSolver;

    /**
     * Creates a new prime implicant computation for a given formula.
     * @param formula the formula
     */
    public NaivePrimeReduction(final Formula formula) {
        final FormulaFactory f = formula.factory();
        this.implicantSolver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        this.implicantSolver.add(formula.negate());
        this.implicateSolver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        this.implicateSolver.add(formula);
    }

    /**
     * Computes a prime implicant from the given implicant for the given formula.
     * Assumption: Given implicant is a satisfying assignment for the formula
     * @param implicant the implicant
     * @return a prime implicant
     */
    public SortedSet<Literal> reduceImplicant(final SortedSet<Literal> implicant) {
        return reduceImplicant(implicant, null);
    }

    /**
     * Computes a prime implicant from the given implicant for the given formula.
     * Assumption: Given implicant is a satisfying assignment for the formula
     * @param implicant the implicant
     * @param handler   the SAT handler
     * @return a prime implicant
     */
    public SortedSet<Literal> reduceImplicant(final SortedSet<Literal> implicant, final SATHandler handler) {
        start(handler);
        final SortedSet<Literal> primeImplicant = new TreeSet<>(implicant);
        for (final Literal lit : implicant) {
            primeImplicant.remove(lit);
            final boolean sat = this.implicantSolver.sat(handler, primeImplicant) == Tristate.TRUE;
            if (aborted(handler)) {
                return null;
            }
            if (sat) {
                primeImplicant.add(lit);
            }
        }
        return primeImplicant;
    }

    /**
     * Computes a prime implicate from the given implicate for the given formula.
     * Assumption: Given implicate is a falsifying assignment for the formula, i.e. a satisfying assignment for the
     * negated formula
     * @param implicate the implicate
     * @return a prime implicate
     */
    public SortedSet<Literal> reduceImplicate(final SortedSet<Literal> implicate) {
        return reduceImplicate(implicate, null);
    }

    /**
     * Computes a prime implicate from the given implicate for the given formula.
     * Assumption: Given implicate is a falsifying assignment for the formula, i.e. a satisfying assignment for the
     * negated formula
     * @param implicate the implicate
     * @param handler   the SAT handler
     * @return a prime implicate
     */
    public SortedSet<Literal> reduceImplicate(final SortedSet<Literal> implicate, final SATHandler handler) {
        start(handler);
        final SortedSet<Literal> primeImplicate = new TreeSet<>(implicate);
        for (final Literal lit : implicate) {
            primeImplicate.remove(lit);
            final List<Literal> assumptions = FormulaHelper.negateLiterals(primeImplicate, ArrayList::new);
            final boolean sat = this.implicateSolver.sat(handler, assumptions) == Tristate.TRUE;
            if (aborted(handler)) {
                return null;
            }
            if (sat) {
                primeImplicate.add(lit);
            }
        }
        return primeImplicate;
    }
}
