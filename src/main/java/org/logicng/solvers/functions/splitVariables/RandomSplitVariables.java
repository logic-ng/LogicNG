package org.logicng.solvers.functions.splitVariables;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

public class RandomSplitVariables implements SplitVariableProvider {
    private final int minNumberOfVars;
    private final int lowerBound;

    public RandomSplitVariables() {
        this.minNumberOfVars = 12;
        this.lowerBound = 50;
    }

    public RandomSplitVariables(final int minNumberOfVars, final int lowerBound) {
        this.minNumberOfVars = minNumberOfVars;
        this.lowerBound = lowerBound;
    }

    @Override
    public SortedSet<Variable> getOrder(final Collection<Formula> formulas, final Collection<Variable> variables) {
        if (variables.size() < this.minNumberOfVars) {
            return Collections.emptySortedSet();
        }
        final int minNumberOfSplitVars = (int) Math.ceil(this.lowerBound * variables.size() / HUNDRED);
        final List<Variable> vars = new ArrayList<>(variables);
        final SortedSet<Variable> splitVars = new TreeSet<>();
        int counterR = 0;
        while (splitVars.size() < minNumberOfSplitVars) {
            splitVars.add(vars.get(counterR));
            counterR++;
        }
        return splitVars;
    }
}
