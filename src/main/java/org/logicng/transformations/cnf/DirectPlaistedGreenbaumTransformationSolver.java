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
import org.logicng.formulas.Equivalence;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Implication;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Not;
import org.logicng.formulas.Or;
import org.logicng.predicates.CNFPredicate;
import org.logicng.predicates.ContainsPBCPredicate;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.sat.MiniSatStyleSolver;
import org.logicng.util.Pair;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * A Plaisted-Greenbaum CNF conversion which is performed directly on the internal SAT solver,
 * not on a formula factory.
 * @version 2.0.0
 * @since 1.6.0
 */
public final class DirectPlaistedGreenbaumTransformationSolver {

    private final Map<Formula, VarCacheEntry> variableCache;
    private final MiniSatStyleSolver solver;
    private final boolean initialPhase;

    /**
     * Constructs a new transformation for a given SAT solver.
     * @param solver       the solver
     * @param initialPhase the initial phase for new variables
     */
    public DirectPlaistedGreenbaumTransformationSolver(final MiniSatStyleSolver solver, final boolean initialPhase) {
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
        final Formula withoutPBCs = formula.holds(ContainsPBCPredicate.get()) ? formula.nnf() : formula;
        if (withoutPBCs.holds(CNFPredicate.get())) {
            addCNF(withoutPBCs, proposition);
        } else {
            final int topLevelVar = computeTransformation(withoutPBCs, true, proposition);
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

    private int computeTransformation(final Formula formula, final boolean polarity, final Proposition proposition) {
        switch (formula.type()) {
            case LITERAL:
                final Literal lit = (Literal) formula;
                return polarity ? solverLiteral(lit.name(), lit.phase()) : solverLiteral(lit.name(), lit.phase()) ^ 1;
            case NOT:
                return computeTransformation(((Not) formula).operand(), !polarity, proposition);
            case OR:
            case AND:
                return handleNary(formula, polarity, proposition);
            case IMPL:
                return handleImplication((Implication) formula, polarity, proposition);
            case EQUIV:
                return handleEquivalence((Equivalence) formula, polarity, proposition);
            default:
                throw new IllegalArgumentException("Could not process the formula type " + formula.type());
        }
    }

    private int handleImplication(final Implication formula, final boolean polarity, final Proposition proposition) {
        final Pair<Boolean, Integer> pgVarResult = getPgVar(formula, polarity);
        if (pgVarResult.first()) {
            return polarity ? pgVarResult.second() : pgVarResult.second() ^ 1;
        }
        final int pgVar = pgVarResult.second();
        if (polarity) {
            // pg => (~left | right) = ~pg | ~left | right
            final int leftPgVarNeg = computeTransformation(formula.left(), false, proposition);
            final int rightPgVarPos = computeTransformation(formula.right(), true, proposition);
            this.solver.addClause(new LNGIntVector(pgVar ^ 1, leftPgVarNeg, rightPgVarPos), proposition);
        } else {
            // (~left | right) => pg = (left & ~right) | pg = (left | pg) & (~right | pg)
            final int leftPgVarPos = computeTransformation(formula.left(), true, proposition);
            final int rightPgVarNeg = computeTransformation(formula.right(), false, proposition);
            this.solver.addClause(new LNGIntVector(new int[]{pgVar, leftPgVarPos}), proposition);
            this.solver.addClause(new LNGIntVector(new int[]{pgVar, rightPgVarNeg}), proposition);
        }
        return polarity ? pgVar : pgVar ^ 1;
    }

    private int handleEquivalence(final Equivalence formula, final boolean polarity, final Proposition proposition) {
        final Pair<Boolean, Integer> pgVarResult = getPgVar(formula, polarity);
        if (pgVarResult.first()) {
            return polarity ? pgVarResult.second() : pgVarResult.second() ^ 1;
        }
        final int pgVar = pgVarResult.second();
        final int leftPgVarPos = computeTransformation(formula.left(), true, proposition);
        final int leftPgVarNeg = computeTransformation(formula.left(), false, proposition);
        final int rightPgVarPos = computeTransformation(formula.right(), true, proposition);
        final int rightPgVarNeg = computeTransformation(formula.right(), false, proposition);
        if (polarity) {
            // pg => (left => right) & (right => left)
            // = (pg & left => right) & (pg & right => left)
            // = (~pg | ~left | right) & (~pg | ~right | left)
            this.solver.addClause(new LNGIntVector(pgVar ^ 1, leftPgVarNeg, rightPgVarPos), proposition);
            this.solver.addClause(new LNGIntVector(pgVar ^ 1, leftPgVarPos, rightPgVarNeg), proposition);
        } else {
            // (left => right) & (right => left) => pg
            // = ~(left => right) | ~(right => left) | pg
            // = left & ~right | right & ~left | pg
            // = (left | right | pg) & (~right | ~left | pg)
            this.solver.addClause(new LNGIntVector(pgVar, leftPgVarPos, rightPgVarPos), proposition);
            this.solver.addClause(new LNGIntVector(pgVar, leftPgVarNeg, rightPgVarNeg), proposition);
        }
        return polarity ? pgVar : pgVar ^ 1;
    }

    private int handleNary(final Formula formula, final boolean polarity, final Proposition proposition) {
        final Pair<Boolean, Integer> pgVarResult = getPgVar(formula, polarity);
        if (pgVarResult.first()) {
            return polarity ? pgVarResult.second() : pgVarResult.second() ^ 1;
        }
        final int pgVar = pgVarResult.second();
        switch (formula.type()) {
            case AND: {
                if (polarity) {
                    // pg => (v1 & ... & vk) = (~pg | v1) & ... & (~pg | vk)
                    for (final Formula op : formula) {
                        // Speed Up: Skip additional auxiliary variables for nested ORs
                        if (op.type() == FType.OR) {
                            final Or or = (Or) op;
                            final LNGIntVector clause = new LNGIntVector(or.numberOfOperands() + 1);
                            clause.push(pgVar ^ 1);
                            for (final Formula opOr : or) {
                                clause.push(computeTransformation(opOr, true, proposition));
                            }
                            this.solver.addClause(clause, proposition);
                        } else {
                            final int opPgVar = computeTransformation(op, true, proposition);
                            this.solver.addClause(new LNGIntVector(new int[]{pgVar ^ 1, opPgVar}), proposition);
                        }
                    }
                } else {
                    // (v1 & ... & vk) => pg = ~v1 | ... | ~vk | pg
                    final LNGIntVector singleClause = new LNGIntVector();
                    singleClause.push(pgVar);
                    for (final Formula op : formula) {
                        final int opPgVar = computeTransformation(op, false, proposition);
                        singleClause.push(opPgVar);
                    }
                    this.solver.addClause(singleClause, proposition);
                }
                break;
            }
            case OR: {
                if (polarity) {
                    // pg => (v1 | ... | vk) = ~pg | v1 | ... | vk
                    final LNGIntVector singleClause = new LNGIntVector();
                    singleClause.push(pgVar ^ 1);
                    for (final Formula op : formula) {
                        final int opPgVar = computeTransformation(op, true, proposition);
                        singleClause.push(opPgVar);
                    }
                    this.solver.addClause(singleClause, proposition);
                } else {
                    // (v1 | ... | vk) => pg = (~v1 | pg) & ... & (~vk | pg)
                    for (final Formula op : formula) {
                        final int opPgVar = computeTransformation(op, false, proposition);
                        this.solver.addClause(new LNGIntVector(new int[]{pgVar, opPgVar}), proposition);
                    }
                }
                break;
            }
            default:
                throw new IllegalArgumentException("Unexpected type: " + formula.type());
        }
        return polarity ? pgVar : pgVar ^ 1;
    }

    private Pair<Boolean, Integer> getPgVar(final Formula formula, final boolean polarity) {
        final VarCacheEntry entry = this.variableCache.computeIfAbsent(formula, i -> new VarCacheEntry(newSolverVariable()));
        final boolean wasCached = entry.setPolarityCached(polarity);
        final int pgVar = entry.pgVar;
        return new Pair<>(wasCached, pgVar);
    }

    private LNGIntVector generateClauseVector(final Collection<Literal> literals) {
        final LNGIntVector clauseVec = new LNGIntVector(literals.size());
        for (final Literal lit : literals) {
            clauseVec.push(solverLiteral(lit.name(), lit.phase()));
        }
        return clauseVec;
    }

    private int solverLiteral(final String name, final boolean phase) {
        int index = this.solver.idxForName(name);
        if (index == -1) {
            index = this.solver.newVar(!this.initialPhase, true);
            this.solver.addName(name, index);
        }
        return phase ? index * 2 : (index * 2) ^ 1;
    }

    private int newSolverVariable() {
        final int index = this.solver.newVar(!this.initialPhase, true);
        final String name = FormulaFactory.CNF_PREFIX + "MINISAT_" + index;
        this.solver.addName(name, index);
        return index * 2;
    }

    private static class VarCacheEntry {
        private final Integer pgVar;
        private boolean posPolarityCached = false;
        private boolean negPolarityCached = false;

        public VarCacheEntry(final Integer pgVar) {
            this.pgVar = pgVar;
        }

        public boolean setPolarityCached(final boolean polarity) {
            final boolean wasCached;
            if (polarity) {
                wasCached = this.posPolarityCached;
                this.posPolarityCached = true;
            } else {
                wasCached = this.negPolarityCached;
                this.negPolarityCached = true;
            }
            return wasCached;
        }
    }
}
