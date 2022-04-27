package org.logicng.solvers.functions.splitVariables;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

public class MostCommonVariables implements SplitVariableProvider {
    private final int minNumberOfVars;
    private final int lowerBound;
    private final int upperBound;

    public MostCommonVariables() {
        this.minNumberOfVars = 12;
        this.lowerBound = 50;
        this.upperBound = 80;
    }

    public MostCommonVariables(final int minNumberOfVars, final int lowerBound, final int upperBound) {
        this.minNumberOfVars = minNumberOfVars;
        this.lowerBound = lowerBound;
        this.upperBound = upperBound;
    }

    @Override
    public SortedSet<Variable> getOrder(final Collection<Formula> formulas, final Collection<Variable> variables) {
        if (variables.size() < this.minNumberOfVars) {
            return Collections.emptySortedSet();
        }
        final Map<Integer, SortedSet<Variable>> occurrence2Vars = getOccurrence2Vars(formulas);
        if (occurrence2Vars == null || occurrence2Vars.isEmpty()) {
            return Collections.emptySortedSet();
        }
        final int minNumberOfSplitVars = (int) Math.ceil(this.lowerBound * variables.size() / HUNDRED);
        final int maxNumberOfSplitVars = (int) Math.floor(this.upperBound * variables.size() / HUNDRED);
        final SortedSet<Variable> splitVars = new TreeSet<>();
        int counter = occurrence2Vars.keySet().stream().mapToInt(i -> i).max().getAsInt();
        while (splitVars.size() < minNumberOfSplitVars) {
            fillSplitVars(occurrence2Vars, counter, minNumberOfSplitVars, maxNumberOfSplitVars, splitVars);
            counter--;
        }
        return splitVars;
    }
}
