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

package org.logicng.transformations;

import org.logicng.collections.LNGIntVector;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;
import org.logicng.formulas.cache.TransformationCacheEntry;
import org.logicng.solvers.datastructures.MSClause;
import org.logicng.solvers.sat.MiniSat2Solver;
import org.logicng.solvers.sat.MiniSatConfig;

import java.util.ArrayList;
import java.util.List;

/**
 * A formula transformation which performs unit propagation.
 * @version 1.3
 * @since 1.2
 */
public final class UnitPropagation implements FormulaTransformation {

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final Formula cached = formula.transformationCacheEntry(TransformationCacheEntry.UNIT_PROPAGATION);
        if (cached != null) {
            return cached;
        }
        final MiniSatPropagator miniSatPropagator = new MiniSatPropagator();
        miniSatPropagator.add(formula);
        final Formula result = miniSatPropagator.propagatedFormula(formula.factory());
        if (cache) {
            formula.setTransformationCacheEntry(TransformationCacheEntry.UNIT_PROPAGATION, result);
        }
        return result;
    }

    /**
     * An extension of Minisat to propagate units on formulas.
     */
    private static class MiniSatPropagator extends MiniSat2Solver {

        /**
         * Constructs a new MiniSatPropagator.
         */
        public MiniSatPropagator() {
            super(MiniSatConfig.builder().incremental(false).build());
        }

        /**
         * Adds an arbitrary formula to the propagator.
         * @param formula the formula
         */
        public void add(final Formula formula) {
            final Formula cnf = formula.cnf();
            switch (cnf.type()) {
                case TRUE:
                    break;
                case FALSE:
                case LITERAL:
                case OR:
                    this.addClause(generateClauseVector(cnf), null);
                    break;
                case AND:
                    for (final Formula op : cnf) {
                        this.addClause(generateClauseVector(op), null);
                    }
                    break;
                default:
                    throw new IllegalStateException("Unexpected formula type in CNF: " + cnf.type());
            }
        }

        /**
         * Performs unit propagation on level 0 and returns the propagated formula.
         * @param f the formula factory
         * @return the propagated formula
         */
        public Formula propagatedFormula(final FormulaFactory f) {
            assert decisionLevel() == 0;
            if (!this.ok || this.propagate() != null) {
                return f.falsum();
            }
            final List<Formula> clauses = new ArrayList<>();
            for (final MSClause clause : this.clauses) {
                clauses.add(clauseToFormula(clause, f));
            }
            for (int i = 0; i < this.trail.size(); i++) {
                clauses.add(solverLiteralToFormula(this.trail.get(i), f));
            }
            return f.and(clauses);
        }

        /**
         * Transforms an solver literal into the corresponding formula literal.
         * @param lit the solver literal
         * @param f   the formula factory
         * @return the formula literal
         */
        private Literal solverLiteralToFormula(final int lit, final FormulaFactory f) {
            return f.literal(this.nameForIdx(var(lit)), !sign(lit));
        }

        /**
         * Transforms a solver clause into a formula, respecting the current solver state.
         * I.e. all falsified literals are removed from the resulting clause and
         * if any literal of the clause is satisfied, the result is {@link org.logicng.formulas.CTrue}.
         * @param clause the solver clause to transform
         * @param f      the formula factory
         * @return the transformed clause
         */
        private Formula clauseToFormula(final MSClause clause, final FormulaFactory f) {
            final List<Literal> literals = new ArrayList<>(clause.size());
            for (int i = 0; i < clause.size(); i++) {
                final int lit = clause.get(i);
                switch (value(lit)) {
                    case TRUE:
                        return f.verum();
                    case UNDEF:
                        literals.add(solverLiteralToFormula(lit, f));
                        break;
                    case FALSE:
                        // ignore this literal
                }
            }
            return f.or(literals);
        }

        /**
         * Generates a solver vector of a clause.
         * @param clause the clause
         * @return the solver vector
         */
        private LNGIntVector generateClauseVector(final Formula clause) {
            final LNGIntVector clauseVec = new LNGIntVector(clause.numberOfOperands());
            for (final Literal lit : clause.literals()) {
                int index = this.idxForName(lit.name());
                if (index == -1) {
                    index = this.newVar(false, false);
                    this.addName(lit.name(), index);
                }
                final int litNum = lit.phase() ? index * 2 : (index * 2) ^ 1;
                clauseVec.push(litNum);
            }
            return clauseVec;
        }
    }
}
