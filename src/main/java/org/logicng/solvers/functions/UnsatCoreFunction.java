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

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.UNSATCore;
import org.logicng.explanations.drup.DRUPTrim;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.propositions.Proposition;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.sat.GlucoseSyrup;
import org.logicng.solvers.sat.MiniCard;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * A solver function for computing the unsatisfiable core on the current solver.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class UnsatCoreFunction implements SolverFunction<UNSATCore<Proposition>> {

    private final static UnsatCoreFunction INSTANCE = new UnsatCoreFunction();

    /**
     * Private empty constructor.  Singleton class.
     */
    private UnsatCoreFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the function.
     * @return the function instance
     */
    public static UnsatCoreFunction get() {
        return INSTANCE;
    }

    @Override
    public UNSATCore<Proposition> apply(final MiniSat solver, final Consumer<Tristate> resultSetter) {
        if (!solver.getConfig().proofGeneration()) {
            throw new IllegalStateException("Cannot generate an unsat core if proof generation is not turned on");
        }
        if (solver.getResult() == TRUE) {
            throw new IllegalStateException("An unsat core can only be generated if the formula is solved and is UNSAT");
        }
        if (solver.getResult() == Tristate.UNDEF) {
            throw new IllegalStateException("Cannot generate an unsat core before the formula was solved.");
        }
        if (solver.underlyingSolver() instanceof MiniCard) {
            throw new IllegalStateException("Cannot compute an unsat core with MiniCard.");
        }
        if (solver.underlyingSolver() instanceof GlucoseSyrup && solver.getConfig().incremental()) {
            throw new IllegalStateException("Cannot compute an unsat core with Glucose in incremental mode.");
        }
        if (solver.isLastComputationWithAssumptions()) {
            throw new IllegalStateException("Cannot compute an unsat core for a computation with assumptions.");
        }

        final DRUPTrim trimmer = new DRUPTrim();

        final Map<Formula, Proposition> clause2proposition = new HashMap<>();
        final LNGVector<LNGIntVector> clauses = new LNGVector<>(solver.underlyingSolver().pgOriginalClauses().size());
        for (final MiniSatStyleSolver.ProofInformation pi : solver.underlyingSolver().pgOriginalClauses()) {
            clauses.push(pi.clause());
            final Formula clause = getFormulaForVector(solver, pi.clause());
            Proposition proposition = pi.proposition();
            if (proposition == null) {
                proposition = new StandardProposition(clause);
            }
            clause2proposition.put(clause, proposition);
        }

        if (containsEmptyClause(clauses)) {
            final Proposition emptyClause = clause2proposition.get(solver.factory().falsum());
            return new UNSATCore<>(Collections.singletonList(emptyClause), true);
        }

        final DRUPTrim.DRUPResult result = trimmer.compute(clauses, solver.underlyingSolver().pgProof());
        if (result.trivialUnsat()) {
            return handleTrivialCase(solver);
        }
        final LinkedHashSet<Proposition> propositions = new LinkedHashSet<>();
        for (final LNGIntVector vector : result.unsatCore()) {
            propositions.add(clause2proposition.get(getFormulaForVector(solver, vector)));
        }
        return new UNSATCore<>(new ArrayList<>(propositions), false);
    }

    private UNSATCore<Proposition> handleTrivialCase(final MiniSat solver) {
        final LNGVector<MiniSatStyleSolver.ProofInformation> clauses = solver.underlyingSolver().pgOriginalClauses();
        for (int i = 0; i < clauses.size(); i++) {
            for (int j = i + 1; j < clauses.size(); j++) {
                if (clauses.get(i).clause().size() == 1 && clauses.get(j).clause().size() == 1
                        && clauses.get(i).clause().get(0) + clauses.get(j).clause().get(0) == 0) {
                    final LinkedHashSet<Proposition> propositions = new LinkedHashSet<>();
                    final Proposition pi = clauses.get(i).proposition();
                    final Proposition pj = clauses.get(j).proposition();
                    propositions.add(pi != null ? pi : new StandardProposition(getFormulaForVector(solver, clauses.get(i).clause())));
                    propositions.add(pj != null ? pj : new StandardProposition(getFormulaForVector(solver, clauses.get(j).clause())));
                    return new UNSATCore<>(new ArrayList<>(propositions), false);
                }
            }
        }
        throw new IllegalStateException("Should be a trivial unsat core, but did not found one.");
    }

    private boolean containsEmptyClause(final LNGVector<LNGIntVector> clauses) {
        for (final LNGIntVector clause : clauses) {
            if (clause.empty()) {
                return true;
            }
        }
        return false;
    }

    private Formula getFormulaForVector(final MiniSat solver, final LNGIntVector vector) {
        final List<Literal> literals = new ArrayList<>(vector.size());
        for (int i = 0; i < vector.size(); i++) {
            final int lit = vector.get(i);
            final String varName = solver.underlyingSolver().nameForIdx(Math.abs(lit) - 1);
            literals.add(solver.factory().literal(varName, lit > 0));
        }
        return solver.factory().or(literals);
    }
}
