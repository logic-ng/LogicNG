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

package org.logicng.knowledgecompilation.dnnf;

import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.solvers.datastructures.MSClause;
import org.logicng.solvers.datastructures.MSVariable;
import org.logicng.solvers.sat.MiniSat2Solver;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collection;
import java.util.List;

/**
 * A variation of the MiniSat solver used during the DNNF compilation process.
 * @version 2.0.0
 * @since 2.0.0
 */
public class DnnfMiniSatStyleSolver extends MiniSat2Solver implements DnnfSatSolver {

    protected boolean newlyImpliedDirty = false;
    protected int assertionLevel = -1;
    protected LNGIntVector lastLearnt = null;
    protected final FormulaFactory f;
    protected final Tristate[] assignment;
    protected final List<Literal> impliedOperands;

    /**
     * Constructs a new DNNF MiniSat solver with the given number of variables.
     * @param f                 the formula factory
     * @param numberOfVariables the number of variables
     */
    public DnnfMiniSatStyleSolver(final FormulaFactory f, final int numberOfVariables) {
        this.f = f;
        this.assignment = new Tristate[2 * numberOfVariables];
        Arrays.fill(this.assignment, Tristate.UNDEF);
        this.impliedOperands = new ArrayList<>();
    }

    @Override
    public boolean start() {
        this.newlyImpliedDirty = true;
        return propagate() == null;
    }

    @Override
    public Tristate valueOf(final int lit) {
        return this.assignment[lit];
    }

    @Override
    public int variableIndex(final Literal lit) {
        return idxForName(lit.name());
    }

    @Override
    public Literal litForIdx(final int var) {
        return this.f.literal(this.idx2name.get(var), true);
    }

    /**
     * Returns the variable index for a given literal.
     * @param lit the literal
     * @return the variable index of the literal
     */
    public static int var(final int lit) {
        return MiniSatStyleSolver.var(lit);
    }

    /**
     * Returns the phase of the given solver literal.
     * @param lit the solver literal
     * @return {@code true} if the literal has a positive phase,
     * {@code false} if the literal has a negative phase (literal is negated)
     */
    public static boolean phase(final int lit) {
        return !sign(lit);
    }

    @Override
    public void add(final Formula formula) {
        final Formula cnf = formula.cnf();
        switch (cnf.type()) {
            case TRUE:
                break;
            case FALSE:
            case LITERAL:
            case OR:
                this.addClause(generateClauseVector(cnf.literals()), null);
                break;
            case AND:
                for (final Formula op : cnf) {
                    this.addClause(generateClauseVector(op.literals()), null);
                }
                break;
            default:
                throw new IllegalArgumentException("Input formula ist not a valid CNF: " + cnf);
        }
    }

    protected LNGIntVector generateClauseVector(final Collection<Literal> literals) {
        final LNGIntVector clauseVec = new LNGIntVector(literals.size());
        for (final Literal lit : literals) {
            int index = idxForName(lit.name());
            if (index == -1) {
                index = newVar(false, true);
                addName(lit.name(), index);
            }
            final int litNum = lit.phase() ? index * 2 : (index * 2) ^ 1;
            clauseVec.push(litNum);
        }
        return clauseVec;
    }

    @Override
    public boolean decide(final int var, final boolean phase) {
        this.newlyImpliedDirty = true;
        final int lit = mkLit(var, !phase);
        this.trailLim.push(this.trail.size());
        uncheckedEnqueue(lit, null);
        return propagateAfterDecide();
    }

    @Override
    public void undoDecide(final int var) {
        this.newlyImpliedDirty = false;
        cancelUntil(this.vars.get(var).level() - 1);
    }

    @Override
    public boolean atAssertionLevel() {
        return decisionLevel() == this.assertionLevel;
    }

    @Override
    public boolean assertCdLiteral() {
        this.newlyImpliedDirty = true;
        if (!atAssertionLevel()) {
            throw new IllegalStateException("assertCdLiteral called although not at assertion level!");
        }

        if (this.lastLearnt.size() == 1) {
            uncheckedEnqueue(this.lastLearnt.get(0), null);
            this.unitClauses.push(this.lastLearnt.get(0));
        } else {
            final MSClause cr = new MSClause(this.lastLearnt, true);
            this.learnts.push(cr);
            attachClause(cr);
            if (!this.incremental) {
                claBumpActivity(cr);
            }
            uncheckedEnqueue(this.lastLearnt.get(0), cr);
        }
        decayActivities();
        return propagateAfterDecide();
    }

    @Override
    public Formula newlyImplied(final BitSet knownVariables) {
        this.impliedOperands.clear();
        if (this.newlyImpliedDirty) {
            final int limit = this.trailLim.empty() ? -1 : this.trailLim.back();
            for (int i = this.trail.size() - 1; i > limit; i--) {
                final int lit = this.trail.get(i);
                if (knownVariables.get(var(lit))) {
                    this.impliedOperands.add(intToLiteral(lit));
                }
            }
        }
        this.newlyImpliedDirty = false;
        return this.f.and(this.impliedOperands);
    }

    protected Literal intToLiteral(final int lit) {
        final String name = nameForIdx(var(lit));
        return this.f.literal(name, !sign(lit));
    }

    protected boolean propagateAfterDecide() {
        final MSClause conflict = propagate();
        if (conflict != null) {
            handleConflict(conflict);
            return false;
        }
        return true;
    }

    @Override
    protected void uncheckedEnqueue(final int lit, final MSClause reason) {
        this.assignment[lit] = Tristate.TRUE;
        this.assignment[lit ^ 1] = Tristate.FALSE;
        super.uncheckedEnqueue(lit, reason);
    }

    @Override
    protected void cancelUntil(final int level) {
        if (decisionLevel() > level) {
            for (int c = this.trail.size() - 1; c >= this.trailLim.get(level); c--) {
                final int l = this.trail.get(c);
                this.assignment[l] = Tristate.UNDEF;
                this.assignment[l ^ 1] = Tristate.UNDEF;
                final int x = var(l);
                final MSVariable v = this.vars.get(x);
                v.assign(Tristate.UNDEF);
                v.setPolarity(sign(this.trail.get(c)));
                insertVarOrder(x);
            }
            this.qhead = this.trailLim.get(level);
            this.trail.removeElements(this.trail.size() - this.trailLim.get(level));
            this.trailLim.removeElements(this.trailLim.size() - level);
        }
    }

    protected void handleConflict(final MSClause conflict) {
        if (decisionLevel() > 0) {
            this.lastLearnt = new LNGIntVector();
            analyze(conflict, this.lastLearnt);
            this.assertionLevel = this.analyzeBtLevel;
        } else {
            // solver unsat
            cancelUntil(0);
            this.lastLearnt = null;
            this.assertionLevel = -1;
        }
    }
}
