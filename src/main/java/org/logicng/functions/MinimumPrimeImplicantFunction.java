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

package org.logicng.functions;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFunction;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.OptimizationFunction;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.transformations.LiteralSubstitution;

import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Computes a minimum-size prime implicant for the given formula.
 * Returns {@code null} if the formula was not satisfiable.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class MinimumPrimeImplicantFunction implements FormulaFunction<SortedSet<Literal>> {

    private static final String POS = "_POS";
    private static final String NEG = "_NEG";
    private static final MinimumPrimeImplicantFunction INSTANCE = new MinimumPrimeImplicantFunction();

    private MinimumPrimeImplicantFunction() {
        // intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static MinimumPrimeImplicantFunction get() {
        return INSTANCE;
    }

    @Override
    public SortedSet<Literal> apply(final Formula formula, final boolean cache) {
        final Formula nnf = formula.nnf();
        final Map<Variable, Literal> newVar2oldLit = new HashMap<>();
        final LiteralSubstitution substitution = new LiteralSubstitution();
        for (final Literal literal : nnf.literals()) {
            final Variable newVar = formula.factory().variable(literal.name() + (literal.phase() ? POS : NEG));
            newVar2oldLit.put(newVar, literal);
            substitution.addSubstitution(literal, newVar);
        }
        final Formula substitued = nnf.transform(substitution);
        final SATSolver solver = MiniSat.miniSat(formula.factory(), MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        solver.add(substitued);
        for (final Literal literal : newVar2oldLit.values()) {
            if (literal.phase() && newVar2oldLit.containsValue(literal.negate())) {
                solver.add(formula.factory().amo(formula.factory().variable(literal.name() + POS), formula.factory().variable(literal.name() + NEG)));
            }
        }

        if (solver.sat() != Tristate.TRUE) {
            return null;
        }
        final Assignment minimumModel = solver.execute(OptimizationFunction.minimize(newVar2oldLit.keySet()));
        final SortedSet<Literal> primeImplicant = new TreeSet<>();
        for (final Variable variable : minimumModel.positiveVariables()) {
            final Literal literal = newVar2oldLit.get(variable);
            if (literal != null) {
                primeImplicant.add(literal);
            }
        }
        return primeImplicant;
    }
}
