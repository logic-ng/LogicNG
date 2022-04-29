package org.logicng.solvers.functions.splitVariableProvider;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A split variable provider which provides random split variables.
 * @version 2.3.0
 * @since 2.3.0
 */
public class RandomSplitVariables extends SplitVariableProvider {

    public RandomSplitVariables() {
        new RandomSplitVariables(this.minNumberOfVars, this.lowerBound);
    }

    public RandomSplitVariables(final int minNumberOfVars, final int lowerBound) {
        this.minNumberOfVars = minNumberOfVars;
        this.lowerBound = lowerBound;
    }

    @Override
    public SortedSet<Variable> getSplitVars(final Collection<Formula> formulas, final Collection<Variable> variables) {
        if (notWorthSplitting(variables)) {
            return Collections.emptySortedSet();
        }
        final List<Variable> vars = new ArrayList<>(variables);
        return new TreeSet<>(vars.subList(0, getMinNumberOfSplitVars(variables)));
    }
}
