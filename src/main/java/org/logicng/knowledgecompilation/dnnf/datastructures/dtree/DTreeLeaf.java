// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.dnnf.datastructures.dtree;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.DnnfSatSolver;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.SortedSet;

/**
 * A leaf in a DTree.
 * @version 2.0.0
 * @since 2.0.0
 */
public class DTreeLeaf extends DTree {

    protected final int id;
    protected final Formula clause;
    protected final int clauseSize;

    protected int[] literals;

    protected final BitSet separatorBitSet = new BitSet();

    protected DnnfSatSolver solver;
    protected final int[] staticClauseIds;

    /**
     * Constructs a new leaf with the given id and clause.
     * @param id     the id
     * @param clause the clause
     */
    public DTreeLeaf(final int id, final Formula clause) {
        this.id = id;
        this.clause = clause;
        this.staticClauseIds = new int[]{id};
        this.clauseSize = clause.variables().size();
        this.staticSeparator = new int[0];
        assert this.clauseSize >= 2;
    }

    @Override
    public void initialize(final DnnfSatSolver solver) {
        this.solver = solver;
        final SortedSet<Literal> lits = this.clause.literals();
        final int size = lits.size();
        this.staticVarSet = new BitSet();
        this.staticVariables = new int[size];
        this.literals = new int[size];
        int i = 0;
        for (final Literal literal : lits) {
            final int var = solver.variableIndex(literal);
            this.staticVarSet.set(var);
            this.staticVariables[i] = var;
            this.literals[i] = MiniSatStyleSolver.mkLit(var, !literal.phase());
            i++;
        }
    }

    @Override
    public int size() {
        return 1;
    }

    @Override
    public SortedSet<Variable> staticVariableSet() {
        return this.clause.variables();
    }

    @Override
    public BitSet dynamicSeparator() {
        return this.separatorBitSet;
    }

    @Override
    public int[] staticClauseIds() {
        return this.staticClauseIds;
    }

    protected boolean isSubsumed() {
        for (final int literal : this.literals) {
            if (this.solver.valueOf(literal) == Tristate.TRUE) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void countUnsubsumedOccurrences(final int[] occurrences) {
        if (!isSubsumed()) {
            for (final int var : this.staticVariables) {
                final int occ = occurrences[var];
                if (occ != -1) {
                    ++occurrences[var];
                }
            }
        }
    }

    @Override
    public int depth() {
        return 1;
    }

    @Override
    public int widestSeparator() {
        return 0;
    }

    @Override
    public List<DTreeLeaf> leafs() {
        final List<DTreeLeaf> result = new ArrayList<>();
        result.add(this);
        return result;
    }

    /**
     * Returns the leaf's clause.
     * @return the leaf's clause
     */
    public Formula clause() {
        return this.clause;
    }

    @Override
    public String toString() {
        return String.format("DTreeLeaf: %d, %s", this.id, this.clause);
    }

    /**
     * Returns the literal integers of the clause.
     * @return literals integers of the clause
     */
    public int[] literals() {
        return this.literals;
    }

    /**
     * Returns the size of the leaf's clause.
     * @return the size of the leaf's clause
     */
    public int clauseSize() {
        return this.clauseSize;
    }

    /**
     * Returns the leaf's id.
     * @return the leaf's id
     */
    public int getId() {
        return this.id;
    }
}
