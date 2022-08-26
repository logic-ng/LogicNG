package org.logicng.solvers.functions.splitVariableProvider;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Supplier;

/**
 * A split variable provider which provides split variables which occur particularly often in the formulas on the solver. The variables occurring in the
 * formulas are sorted by their occurrence. This provider returns those variables with the biggest occurrence.
 * @version 2.3.0
 * @since 2.3.0
 */
public class MostCommonVariableProvider extends SplitVariableProvider {

    public MostCommonVariableProvider() {
        super();
    }

    public MostCommonVariableProvider(final int minNumberOfVars, final int lowerBound, final int upperBound) {
        super(minNumberOfVars, lowerBound, upperBound);
    }

    @Override
    public SortedSet<Variable> getSplitVars(final Supplier<Set<Formula>> formulasSupplier, final Collection<Variable> variables) {
        if (notWorthSplitting(variables)) {
            return Collections.emptySortedSet();
        }
        final Map<Integer, SortedSet<Variable>> occurrence2Vars = getOccurrence2Vars(formulasSupplier.get(), variables);
        if (occurrence2Vars == null || occurrence2Vars.isEmpty()) {
            return Collections.emptySortedSet();
        }
        final int minNumberOfSplitVars = getMinNumberOfSplitVars(variables);
        final int maxNumberOfSplitVars = getMaxNumberOfSplitVars(variables);
        final SortedSet<Variable> splitVars = new TreeSet<>();
        if (!occurrence2Vars.keySet().stream().mapToInt(i -> i).max().isPresent()) {
            throw new IllegalStateException("Max element not available in map");
        }
        int counter = occurrence2Vars.keySet().stream().mapToInt(i -> i).max().getAsInt();
        while (splitVars.size() < minNumberOfSplitVars) {
            fillSplitVars(occurrence2Vars, counter, minNumberOfSplitVars, maxNumberOfSplitVars, splitVars);
            counter--;
        }
        return splitVars;
    }
}
