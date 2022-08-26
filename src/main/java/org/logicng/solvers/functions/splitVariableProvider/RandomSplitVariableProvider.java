package org.logicng.solvers.functions.splitVariableProvider;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Supplier;

/**
 * A split variable provider which provides random split variables.
 * @version 2.3.0
 * @since 2.3.0
 */
public class RandomSplitVariableProvider extends SplitVariableProvider {
    Random random = new Random(0);

    public RandomSplitVariableProvider() {
        super();
    }

    public RandomSplitVariableProvider(final int minNumberOfVars, final int lowerBound, final int randomSeed) {
        super(minNumberOfVars, lowerBound, 100);
        this.random = new Random(randomSeed);
    }

    @Override
    public SortedSet<Variable> getSplitVars(final Supplier<Set<Formula>> formulasSupplier, final Collection<Variable> variables) {
        if (notWorthSplitting(variables)) {
            return Collections.emptySortedSet();
        }
        final List<Variable> vars = new ArrayList<>(variables);
        Collections.shuffle(vars, this.random);
        return new TreeSet<>(vars.subList(0, getMinNumberOfSplitVars(variables)));
    }

    @Override
    public String toString() {
        return "RandomSplitVariableProvider{" +
                ", minNumberOfVars=" + minNumberOfVars +
                ", lowerBound=" + lowerBound +
                ", upperBound=" + upperBound +
                '}';
    }
}
