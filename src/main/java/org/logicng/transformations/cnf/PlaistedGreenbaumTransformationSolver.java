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

package org.logicng.transformations.cnf;

import org.logicng.collections.LNGIntVector;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.predicates.CNFPredicate;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * A Plaisted-Greenbaum CNF conversion which is performed directly on the internal SAT solver,
 * not on a formula factory.
 * @version 2.0.0
 * @since 1.6.0
 */
public final class PlaistedGreenbaumTransformationSolver {

    private final Map<Formula, Integer> variableCache;
    private final MiniSatStyleSolver solver;
    private final boolean initialPhase;

    /**
     * Constructs a new transformation for a given SAT solver.
     * @param solver       the solver
     * @param initialPhase the initial phase for new variables
     */
    public PlaistedGreenbaumTransformationSolver(final MiniSatStyleSolver solver, final boolean initialPhase) {
        this.variableCache = new HashMap<>();
        this.solver = solver;
        this.initialPhase = initialPhase;
    }

    /**
     * Adds the CNF of the given formula (and its optional proposition) to the solver,
     * @param formula     the formula to add to the solver
     * @param proposition the optional proposition of the formula
     */
    public void addCNFtoSolver(final Formula formula, final Proposition proposition) {
        final Formula nnf = formula.nnf();
        if (nnf.holds(CNFPredicate.get())) {
            addCNF(nnf, proposition);
        } else {
            final int topLevelVar = computeTransformation(nnf, proposition);
            this.solver.addClause(topLevelVar, proposition);
        }
    }

    /**
     * Clears the cache.
     */
    public void clearCache() {
        this.variableCache.clear();
    }

    private void addCNF(final Formula cnf, final Proposition proposition) {
        switch (cnf.type()) {
            case TRUE:
                break;
            case FALSE:
            case LITERAL:
            case OR:
                this.solver.addClause(generateClauseVector(cnf.literals()), proposition);
                break;
            case AND:
                for (final Formula clause : cnf) {
                    this.solver.addClause(generateClauseVector(clause.literals()), proposition);
                }
                break;
            default:
                throw new IllegalArgumentException("Input formula ist not a valid CNF: " + cnf);
        }
    }

    private int computeTransformation(final Formula formula, final Proposition proposition) {
        switch (formula.type()) {
            case LITERAL:
                return solverLiteral((Literal) formula);
            case OR:
            case AND:
                return handleNary(formula, proposition);
            default:
                throw new IllegalArgumentException("Could not process the formula type " + formula.type());
        }
    }

    private int handleNary(final Formula formula, final Proposition proposition) {
        Integer pgVar = this.variableCache.get(formula);
        if (pgVar != null) {
            return pgVar;
        } else {
            pgVar = newSolverVariable();
            this.variableCache.put(formula, pgVar);
        }
        switch (formula.type()) {
            case AND: {
                for (final Formula op : formula) {
                    final int opPgVar = computeTransformation(op, proposition);
                    this.solver.addClause(new LNGIntVector(new int[]{pgVar ^ 1, opPgVar}), proposition);
                }
                break;
            }
            case OR: {
                final LNGIntVector singleClause = new LNGIntVector();
                singleClause.push(pgVar ^ 1);
                for (final Formula op : formula) {
                    final int opPgVar = computeTransformation(op, proposition);
                    singleClause.push(opPgVar);
                }
                this.solver.addClause(singleClause, proposition);
                break;
            }
            default:
                throw new IllegalArgumentException("not yet implemented");
        }
        return pgVar;
    }

    private LNGIntVector generateClauseVector(final Collection<Literal> literals) {
        final LNGIntVector clauseVec = new LNGIntVector(literals.size());
        for (final Literal lit : literals) {
            clauseVec.push(solverLiteral(lit));
        }
        return clauseVec;
    }

    private int solverLiteral(final Literal lit) {
        int index = this.solver.idxForName(lit.name());
        if (index == -1) {
            index = this.solver.newVar(!this.initialPhase, true);
            this.solver.addName(lit.name(), index);
        }
        return lit.phase() ? index * 2 : (index * 2) ^ 1;
    }

    private int newSolverVariable() {
        final int index = this.solver.newVar(!this.initialPhase, true);
        final String name = FormulaFactory.CNF_PREFIX + "MINISAT_" + index;
        this.solver.addName(name, index);
        return index * 2;
    }
}
