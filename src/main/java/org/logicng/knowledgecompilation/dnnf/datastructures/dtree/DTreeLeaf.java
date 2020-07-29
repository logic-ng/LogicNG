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
    protected int[] dynamicVarSetHelper;

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
        this.dynamicVarSetHelper = new int[size];
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
