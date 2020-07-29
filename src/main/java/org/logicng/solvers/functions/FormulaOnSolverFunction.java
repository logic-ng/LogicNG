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

import static org.logicng.datastructures.Tristate.TRUE;

import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CFalse;
import org.logicng.formulas.CType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.datastructures.MSClause;
import org.logicng.solvers.datastructures.MSVariable;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

/**
 * A solver function for getting the current formula on the solver.
 * <p>
 * Note that this formula is usually syntactically different to the
 * formulas which were actually added to the solver, since the formulas
 * are added as CNF and may be simplified or even removed depending on
 * the state of the solver. Furthermore the solver might add learnt
 * clauses or propagate literals.
 * <p>
 * If the formula on the solver is known to be unsatisfiable, this
 * function will add {@link CFalse falsum} to the returned set of
 * formulas. However, as long as {@link SATSolver#sat()} was not called
 * on the current solver state, the absence of {@link CFalse falsum}
 * does not imply that the formula is satisfiable.
 * <p>
 * Also note that formulas are not added to the solver as soon as the
 * solver is known be unsatisfiable.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class FormulaOnSolverFunction implements SolverFunction<Set<Formula>> {

    private final static FormulaOnSolverFunction INSTANCE = new FormulaOnSolverFunction();

    /**
     * Private empty constructor.  Singleton class.
     */
    private FormulaOnSolverFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the function.
     * @return the function instance
     */
    public static FormulaOnSolverFunction get() {
        return INSTANCE;
    }

    @Override
    public Set<Formula> apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        final FormulaFactory f = solver.factory();
        final Set<Formula> formulas = new LinkedHashSet<>();
        for (final MSClause clause : solver.underlyingSolver().clauses()) {
            final List<Literal> lits = new ArrayList<>();
            for (int i = 0; i < clause.size(); i++) {
                final int litInt = clause.get(i);
                lits.add(f.literal(solver.underlyingSolver().nameForIdx(litInt >> 1), (litInt & 1) != 1));
            }
            if (!clause.isAtMost()) {
                formulas.add(f.clause(lits));
            } else {
                final int rhs = clause.size() + 1 - clause.atMostWatchers();
                final List<Variable> vars = new ArrayList<>();
                for (final Literal lit : lits) {
                    vars.add(lit.variable());
                }
                formulas.add(f.cc(CType.LE, rhs, vars));
            }
        }
        final LNGVector<MSVariable> variables = solver.underlyingSolver().variables();
        for (int i = 0; i < variables.size(); i++) {
            final MSVariable var = variables.get(i);
            if (var.level() == 0) {
                formulas.add(f.literal(solver.underlyingSolver().nameForIdx(i), var.assignment() == TRUE));
            }
        }
        if (!solver.underlyingSolver().ok()) {
            formulas.add(f.falsum());
        }
        return formulas;
    }
}
