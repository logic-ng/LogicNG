package org.logicng.knowledgecompilation.dnnf.datastructures.dtree;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.dnnf.DNNFSATSolver;
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

    protected DNNFSATSolver solver;
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
    public void initialize(final DNNFSATSolver solver) {
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
    public void dynamicVarSet(final BitSet vars) {
        if (this.clauseSize < 2) {
            return;
        }
        if (this.clauseSize == 2) {
            dynamicVarSet2(vars);
        } else {
            dynamicVarSetN(vars);
        }
    }

    protected boolean dynamicVarSet2(final BitSet vars) {
        final int lit0 = this.literals[0];
        final Tristate lit0Val = this.solver.valueOf(lit0);
        if (lit0Val == Tristate.TRUE) {
            return true;
        }
        final int lit1 = this.literals[1];
        final Tristate lit1Val = this.solver.valueOf(lit1);
        if (lit1Val == Tristate.TRUE) {
            return true;
        }
        if (lit0Val == Tristate.UNDEF) {
            vars.set(MiniSatStyleSolver.var(lit0));
        }
        if (lit1Val == Tristate.UNDEF) {
            vars.set(MiniSatStyleSolver.var(lit1));
        }
        return false;
    }

    protected boolean dynamicVarSetN(final BitSet vars) {
        int toAdd = 0;
        for (int i = 0; i < this.literals.length; i++) {
            final int literal = this.literals[i];
            switch (this.solver.valueOf(literal)) {
                case TRUE:
                    if (i > 0) {
                        this.literals[i] = this.literals[0];
                        this.literals[0] = literal;
                    }
                    return true;
                case UNDEF:
                    this.dynamicVarSetHelper[toAdd++] = MiniSatStyleSolver.var(literal);
            }
        }
        for (int i = 0; i < toAdd; i++) {
            vars.set(this.dynamicVarSetHelper[i]);
        }
        return false;
    }

    @Override
    public BitSet dynamicSeparator() {
        return this.separatorBitSet;
    }

    @Override
    public int[] staticClauseIds() {
        return this.staticClauseIds;
    }

    @Override
    public void cacheKey(final BitSet key, final int numberOfVariables) {
        final boolean subsumed = this.clauseSize == 2 ? dynamicVarSet2(key) : dynamicVarSetN(key);
        if (!subsumed) {
            key.set(numberOfVariables + this.id);
        }
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
