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

package org.logicng.datastructures;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A Boolean assignment.
 * <p>
 * Note: the internal data structure is a plain list - no checking of the model is performed e.g. if
 * contradictory literals are added. Since assignments are used e.g. in the model enumeration of the SAT solvers these
 * checks would be too costly.
 * @version 2.0.0
 * @since 1.0
 */
public final class Assignment {

    private final List<Variable> negVars;
    private Collection<Variable> pos;
    private Collection<Literal> neg;
    private boolean fastEvaluable;

    /**
     * Constructs a new empty assignment (without fast evaluation).
     */
    public Assignment() {
        this(false);
    }

    /**
     * Constructs a new empty assignment.
     * @param fastEvaluable indicates whether this assignment should be evaluable fast.  If this parameter is set to
     *                      {@code true} the internal data structures will be optimized for fast evaluation but
     *                      creation of the object or adding literals can take longer.
     */
    public Assignment(final boolean fastEvaluable) {
        this.fastEvaluable = fastEvaluable;
        this.negVars = new ArrayList<>();
        if (!fastEvaluable) {
            this.pos = new ArrayList<>();
            this.neg = new ArrayList<>();
        } else {
            this.pos = new HashSet<>();
            this.neg = new HashSet<>();
        }
    }

    /**
     * Constructs a new assignment for a given collection of literals (without fast evaluation).
     * @param lits a new assignment for a given collection of literals
     */
    public Assignment(final Collection<? extends Literal> lits) {
        this(lits, false);
    }

    /**
     * Constructs a new assignment for a given array of literals (without fast evaluation).
     * @param lits a new assignment for a given array of literals
     */
    public Assignment(final Literal... lits) {
        this(false);
        for (final Literal lit : lits) {
            addLiteral(lit);
        }
    }

    /**
     * Constructs a new assignment for a given collection of literals.
     * @param lits          a new assignment for a given collection of literals
     * @param fastEvaluable indicates whether this assignment should be evaluable fast.  If this parameter is set to
     *                      {@code true} the internal data structures will be optimized for fast evaluation but
     *                      creation of the object or adding literals can take longer.
     */
    public Assignment(final Collection<? extends Literal> lits, final boolean fastEvaluable) {
        this(fastEvaluable);
        for (final Literal lit : lits) {
            addLiteral(lit);
        }
    }

    /**
     * Constructs a new assignment with a single literal assignment (without fast evaluation).
     * @param lit the literal
     */
    public Assignment(final Literal lit) {
        this(lit, false);
    }

    /**
     * Constructs a new assignment with a single literal assignment.
     * @param lit           the literal
     * @param fastEvaluable indicates whether this assignment should be evaluable fast.  If this parameter is set to
     *                      {@code true} the internal data structures will be optimized for fast evaluation but
     *                      creation of the object or adding literals can take longer.
     */
    public Assignment(final Literal lit, final boolean fastEvaluable) {
        this(fastEvaluable);
        addLiteral(lit);
    }

    /**
     * Converts this assignment to a fast evaluable assignment.
     */
    public void convertToFastEvaluable() {
        if (!this.fastEvaluable) {
            this.pos = new HashSet<>(this.pos);
            this.neg = new HashSet<>(this.neg);
            this.fastEvaluable = true;
        }
    }

    /**
     * Returns whether this assignment is fast evaluable or not.
     * @return {@code true} if this assignment is fast evaluable, {@code false} otherwise
     */
    public boolean fastEvaluable() {
        return this.fastEvaluable;
    }

    /**
     * Returns the number of literals in this assignment.
     * @return the number of literals in this assignment
     */
    public int size() {
        return this.pos.size() + this.neg.size();
    }

    /**
     * Returns the positive literals of this assignment as variables.
     * @return the positive literals of this assignment
     */
    public List<Variable> positiveVariables() {
        return this.fastEvaluable ? Collections.unmodifiableList(new ArrayList<>(this.pos)) : Collections.unmodifiableList((List<Variable>) this.pos);
    }

    /**
     * Returns the negative literals of this assignment.
     * @return the negative literals of this assignment
     */
    public List<Literal> negativeLiterals() {
        return this.fastEvaluable ? Collections.unmodifiableList(new ArrayList<>(this.neg)) : Collections.unmodifiableList((List<Literal>) this.neg);
    }

    /**
     * Returns the negative literals of this assignment as variables.
     * @return the negative literals of this assignment
     */
    public List<Variable> negativeVariables() {
        return Collections.unmodifiableList(this.negVars);
    }

    /**
     * Returns all literals of this assignment.
     * @return all literals of this assignment
     */
    public SortedSet<Literal> literals() {
        final SortedSet<Literal> set = new TreeSet<>();
        set.addAll(this.pos);
        set.addAll(this.neg);
        return set;
    }

    /**
     * Add a single literal to this assignment.
     * @param lit the literal
     */
    public void addLiteral(final Literal lit) {
        if (lit.phase()) {
            this.pos.add(lit.variable());
        } else {
            this.neg.add(lit);
            this.negVars.add(lit.variable());
        }
    }

    /**
     * Evaluates a given literal.  A literal not covered by the assignment evaluates
     * to {@code false} if it is positive, otherwise it evaluates to {@code true}.
     * @param lit the literal
     * @return the evaluation of the literal
     */
    public boolean evaluateLit(final Literal lit) {
        return lit.phase() ? this.pos.contains(lit.variable()) : this.neg.contains(lit) || !this.pos.contains(lit.variable());
    }

    /**
     * Restricts a given literal to a constant.  Returns the literal itself, if the literal's variable is not known.
     * @param lit the literal
     * @return the restriction of the literal or the literal itself, if the literal's variable is not known
     */
    public Formula restrictLit(final Literal lit) {
        final FormulaFactory f = lit.factory();
        final Variable var = lit.variable();
        if (this.pos.contains(var)) {
            return f.constant(lit.phase());
        }
        if (this.neg.contains(var.negate())) {
            return f.constant(!lit.phase());
        }
        return lit;
    }

    /**
     * Returns the assignment as a formula.
     * @param f the formula factory
     * @return the assignment as a formula
     */
    public Formula formula(final FormulaFactory f) {
        return f.and(this.literals());
    }

    /**
     * Creates the blocking clause for this assignment.
     * @param f the formula factory
     * @return the blocking clause for this assignment
     */
    public Formula blockingClause(final FormulaFactory f) {
        final List<Literal> ops = new ArrayList<>();
        for (final Literal lit : this.pos) {
            ops.add(lit.negate());
        }
        for (final Literal lit : this.neg) {
            ops.add(lit.negate());
        }
        return f.or(ops);
    }

    /**
     * Creates the blocking clause for this assignment wrt. a given set of literals.  If the set is {@code null},
     * all literals are considered relevant.
     * @param f        the formula factory
     * @param literals the set of literals
     * @return the blocking clause for this assignment
     */
    public Formula blockingClause(final FormulaFactory f, final Collection<? extends Literal> literals) {
        if (literals == null) {
            return blockingClause(f);
        }
        final List<Literal> ops = new ArrayList<>();
        for (final Literal lit : literals) {
            final Variable var = lit.variable();
            final Literal negatedVar = var.negate();
            if (this.pos.contains(var)) {
                ops.add(negatedVar);
            } else if (this.neg.contains(negatedVar)) {
                ops.add(var);
            }
        }
        return f.or(ops);
    }

    @Override
    public int hashCode() {
        return Objects.hash(new HashSet<>(this.pos), new HashSet<>(this.neg));
    }

    @Override
    public boolean equals(final Object other) {
        if (other == null) {
            return false;
        }
        if (this == other) {
            return true;
        }
        if (this.getClass() == other.getClass()) {
            final Assignment o = (Assignment) other;
            return Objects.equals(new HashSet<>(this.pos), new HashSet<>(o.pos))
                    && Objects.equals(new HashSet<>(this.neg), new HashSet<>(o.neg));
        }
        return false;
    }

    @Override
    public String toString() {
        return String.format("Assignment{pos=%s, neg=%s}", this.pos, this.neg);
    }
}
