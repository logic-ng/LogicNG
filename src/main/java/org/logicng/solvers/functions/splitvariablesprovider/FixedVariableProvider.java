package org.logicng.solvers.functions.splitvariablesprovider;

import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;

import java.util.Collection;
import java.util.SortedSet;

/**
 * A split variable provider for which the variables are fixed.
 * @version 2.4.0
 * @since 2.4.0
 */
public class FixedVariableProvider extends SplitVariableProvider {

    private final SortedSet<Variable> splitVariables;

    /**
     * Creates a new split variables provider returning always the given split variables.
     * @param splitVariables the variables to be returned in {@link #splitVariables}
     */
    public FixedVariableProvider(final SortedSet<Variable> splitVariables) {
        this.splitVariables = splitVariables;
    }

    @Override
    public SortedSet<Variable> getSplitVars(final SATSolver solver, final Collection<Variable> variables) {
        return this.splitVariables;
    }

    @Override
    public String toString() {
        return "FixedVariableProvider{" +
                "splitVariables=" + this.splitVariables +
                '}';
    }
}
