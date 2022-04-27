package org.logicng.solvers.functions.splitVariables;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.Collection;
import java.util.Collections;
import java.util.SortedSet;

public class FixedVariables implements SplitVariableProvider {
    private final int minNumberOfVars;
    private final SortedSet<Variable> splitVariables;

    public FixedVariables(final SortedSet<Variable> splitVariables) {
        this.minNumberOfVars = 12;
        this.splitVariables = splitVariables;
    }

    public FixedVariables(final int minNumberOfVars, final SortedSet<Variable> splitVariables) {
        this.minNumberOfVars = minNumberOfVars;
        this.splitVariables = splitVariables;
    }

    @Override
    public SortedSet<Variable> getOrder(final Collection<Formula> formulas, final Collection<Variable> variables) {
        return this.splitVariables.size() >= this.minNumberOfVars ? this.splitVariables : Collections.emptySortedSet();
    }
}
