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
public final class PlaistedGreenbaumTransformationSolver {

    private final boolean performNNF;
    private final Map<Formula, VarCacheEntry> variableCache;
    private final MiniSatStyleSolver solver;
    private final boolean initialPhase;

    /**
     * Constructs a new transformation for a given SAT solver.
     * @param performNNF   flag whether an NNF transformation should be performed on the input formula
     * @param solver       the solver
     * @param initialPhase the initial phase for new variables
     */
    public PlaistedGreenbaumTransformationSolver(final boolean performNNF, final MiniSatStyleSolver solver, final boolean initialPhase) {
        this.performNNF = performNNF;
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
        final Formula workingFormula = this.performNNF ? formula.nnf() : formula;
        final Formula withoutPBCs = !this.performNNF && workingFormula.holds(ContainsPBCPredicate.get()) ? workingFormula.nnf() : workingFormula;
        if (withoutPBCs.holds(CNFPredicate.get())) {
            addCNF(withoutPBCs, proposition);
        } else {
            final LNGIntVector topLevelVars = computeTransformation(withoutPBCs, true, proposition, true);
            if (topLevelVars != null) {
                this.solver.addClause(topLevelVars, proposition);
            }
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

    private LNGIntVector computeTransformation(final Formula formula, final boolean polarity, final Proposition proposition, final boolean topLevel) {
        switch (formula.type()) {
            case LITERAL:
                final Literal lit = (Literal) formula;
                return polarity ? vector(solverLiteral(lit.name(), lit.phase())) : vector(solverLiteral(lit.name(), lit.phase()) ^ 1);
            case NOT:
                return computeTransformation(((Not) formula).operand(), !polarity, proposition, topLevel);
            case OR:
            case AND:
                return handleNary(formula, polarity, proposition, topLevel);
            case IMPL:
                return handleImplication((Implication) formula, polarity, proposition, topLevel);
            case EQUIV:
                return handleEquivalence((Equivalence) formula, polarity, proposition, topLevel);
            default:
                throw new IllegalArgumentException("Could not process the formula type " + formula.type());
        }
    }

    private LNGIntVector handleImplication(final Implication formula, final boolean polarity, final Proposition proposition, final boolean topLevel) {
        final boolean skipPg = polarity || topLevel;
        final Pair<Boolean, Integer> pgVarResult = skipPg ? new Pair<>(false, null) : getPgVar(formula, polarity);
        if (pgVarResult.first()) {
            return polarity ? vector(pgVarResult.second()) : vector(pgVarResult.second() ^ 1);
        }
        final int pgVar = skipPg ? -1 : pgVarResult.second();
        if (polarity) {
            // pg => (~left | right) = ~pg | ~left | right
            // Speed-Up: Skip pg var
            final LNGIntVector leftPgVarNeg = computeTransformation(formula.left(), false, proposition, false);
            final LNGIntVector rightPgVarPos = computeTransformation(formula.right(), true, proposition, false);
            return vector(leftPgVarNeg, rightPgVarPos);
        } else {
            // (~left | right) => pg = (left & ~right) | pg = (left | pg) & (~right | pg)
            final LNGIntVector leftPgVarPos = computeTransformation(formula.left(), true, proposition, topLevel);
            final LNGIntVector rightPgVarNeg = computeTransformation(formula.right(), false, proposition, topLevel);
            if (topLevel) {
                if (leftPgVarPos != null) {
                    this.solver.addClause(leftPgVarPos, proposition);
                }
                if (rightPgVarNeg != null) {
                    this.solver.addClause(rightPgVarNeg, proposition);
                }
                return null;
            } else {
                this.solver.addClause(vector(pgVar, leftPgVarPos), proposition);
                this.solver.addClause(vector(pgVar, rightPgVarNeg), proposition);
                return vector(pgVar ^ 1);
            }
        }
    }

    private LNGIntVector handleEquivalence(final Equivalence formula, final boolean polarity, final Proposition proposition, final boolean topLevel) {
        final boolean skipPg = topLevel;
        final Pair<Boolean, Integer> pgVarResult = skipPg ? new Pair<>(false, null) : getPgVar(formula, polarity);
        if (pgVarResult.first()) {
            return polarity ? vector(pgVarResult.second()) : vector(pgVarResult.second() ^ 1);
        }
        final int pgVar = skipPg ? -1 : pgVarResult.second();
        final LNGIntVector leftPgVarPos = computeTransformation(formula.left(), true, proposition, false);
        final LNGIntVector leftPgVarNeg = computeTransformation(formula.left(), false, proposition, false);
        final LNGIntVector rightPgVarPos = computeTransformation(formula.right(), true, proposition, false);
        final LNGIntVector rightPgVarNeg = computeTransformation(formula.right(), false, proposition, false);
        if (polarity) {
            // pg => (left => right) & (right => left)
            // = (pg & left => right) & (pg & right => left)
            // = (~pg | ~left | right) & (~pg | ~right | left)
            if (topLevel) {
                this.solver.addClause(vector(leftPgVarNeg, rightPgVarPos), proposition);
                this.solver.addClause(vector(leftPgVarPos, rightPgVarNeg), proposition);
                return null;
            } else {
                this.solver.addClause(vector(pgVar ^ 1, leftPgVarNeg, rightPgVarPos), proposition);
                this.solver.addClause(vector(pgVar ^ 1, leftPgVarPos, rightPgVarNeg), proposition);
            }
        } else {
            // (left => right) & (right => left) => pg
            // = ~(left => right) | ~(right => left) | pg
            // = left & ~right | right & ~left | pg
            // = (left | right | pg) & (~right | ~left | pg)
            if (topLevel) {
                this.solver.addClause(vector(leftPgVarPos, rightPgVarPos), proposition);
                this.solver.addClause(vector(leftPgVarNeg, rightPgVarNeg), proposition);
                return null;
            } else {
                this.solver.addClause(vector(pgVar, leftPgVarPos, rightPgVarPos), proposition);
                this.solver.addClause(vector(pgVar, leftPgVarNeg, rightPgVarNeg), proposition);
            }
        }
        return polarity ? vector(pgVar) : vector(pgVar ^ 1);
    }

    private LNGIntVector handleNary(final Formula formula, final boolean polarity, final Proposition proposition, final boolean topLevel) {
        final boolean skipPg = topLevel || formula.type() == FType.AND && !polarity || formula.type() == FType.OR && polarity;
        final Pair<Boolean, Integer> pgVarResult = skipPg ? new Pair<>(false, null) : getPgVar(formula, polarity);
        if (pgVarResult.first()) {
            return polarity ? vector(pgVarResult.second()) : vector(pgVarResult.second() ^ 1);
        }
        final int pgVar = skipPg ? -1 : pgVarResult.second();
        switch (formula.type()) {
            case AND: {
                if (polarity) {
                    // pg => (v1 & ... & vk) = (~pg | v1) & ... & (~pg | vk)
                    for (final Formula op : formula) {
                        final LNGIntVector opPgVars = computeTransformation(op, true, proposition, topLevel);
                        if (topLevel) {
                            if (opPgVars != null) {
                                this.solver.addClause(opPgVars, proposition);
                            }
                        } else {
                            this.solver.addClause(vector(pgVar ^ 1, opPgVars), proposition);
                        }
                    }
                    if (topLevel) {
                        return null;
                    }
                } else {
                    // (v1 & ... & vk) => pg = ~v1 | ... | ~vk | pg
                    // Speed-Up: Skip pg var
                    final LNGIntVector singleClause = new LNGIntVector();
                    for (final Formula op : formula) {
                        final LNGIntVector opPgVars = computeTransformation(op, false, proposition, false);
                        for (int i = 0; i < opPgVars.size(); i++) {
                            singleClause.push(opPgVars.get(i));
                        }
                    }
                    return singleClause;
                }
                break;
            }
            case OR: {
                if (polarity) {
                    // pg => (v1 | ... | vk) = ~pg | v1 | ... | vk
                    // Speed-Up: Skip pg var
                    final LNGIntVector singleClause = new LNGIntVector();
                    for (final Formula op : formula) {
                        final LNGIntVector opPgVars = computeTransformation(op, true, proposition, false);
                        for (int i = 0; i < opPgVars.size(); i++) {
                            singleClause.push(opPgVars.get(i));
                        }
                    }
                    return singleClause;
                } else {
                    // (v1 | ... | vk) => pg = (~v1 | pg) & ... & (~vk | pg)
                    for (final Formula op : formula) {
                        final LNGIntVector opPgVars = computeTransformation(op, false, proposition, topLevel);
                        if (topLevel) {
                            if (opPgVars != null) {
                                this.solver.addClause(opPgVars, proposition);
                            }
                        } else {
                            this.solver.addClause(vector(pgVar, opPgVars), proposition);
                        }
                    }
                    if (topLevel) {
                        return null;
                    }
                }
                break;
            }
            default:
                throw new IllegalArgumentException("Unexpected type: " + formula.type());
        }
        return polarity ? vector(pgVar) : vector(pgVar ^ 1);
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
            clauseVec.unsafePush(solverLiteral(lit.name(), lit.phase()));
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

    private static LNGIntVector vector(final int... elts) {
        return new LNGIntVector(elts);
    }

    private static LNGIntVector vector(final LNGIntVector a, final LNGIntVector b) {
        final LNGIntVector result = new LNGIntVector(a.size() + b.size());
        for (int i = 0; i < a.size(); i++) {
            result.unsafePush(a.get(i));
        }
        for (int i = 0; i < b.size(); i++) {
            result.unsafePush(b.get(i));
        }
        return result;
    }

    private static LNGIntVector vector(final int elt, final LNGIntVector a) {
        final LNGIntVector result = new LNGIntVector(a.size() + 1);
        result.unsafePush(elt);
        for (int i = 0; i < a.size(); i++) {
            result.unsafePush(a.get(i));
        }
        return result;
    }

    private static LNGIntVector vector(final int elt, final LNGIntVector a, final LNGIntVector b) {
        final LNGIntVector result = new LNGIntVector(a.size() + b.size() + 1);
        result.unsafePush(elt);
        for (int i = 0; i < a.size(); i++) {
            result.unsafePush(a.get(i));
        }
        for (int i = 0; i < b.size(); i++) {
            result.unsafePush(b.get(i));
        }
        return result;
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