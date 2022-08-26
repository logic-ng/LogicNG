package org.logicng.solvers.functions.splitVariableProvider;

import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A split variable provider which provides random split variables.
 * @version 2.4.0
 * @since 2.4.0
 */
public class RandomSplitVariableProvider extends SplitVariableProviderWithTakeRate {
    private final Random random;

    /**
     * Creates a new random split variable provider with the given seed and the take rate.
     * <p>
     * The take rate specifies the number of variables which should be returned in {@link #getSplitVars}.
     * So the result will contain {@code (int) (variables.size() * takeRate)} variables.
     * @param randomSeed the seed for the random
     * @param takeRate   the take rate, must be between 0 and 1 (each inclusive)
     */
    public RandomSplitVariableProvider(final int randomSeed, final double takeRate) {
        super(takeRate);
        this.random = new Random(randomSeed);
    }

    @Override
    public SortedSet<Variable> getSplitVars(final SATSolver solver, final Collection<Variable> variables) {
        final List<Variable> vars = new ArrayList<>(variables);
        Collections.shuffle(vars, this.random);
        return new TreeSet<>(vars.subList(0, numberOfVariablesToChoose(vars)));
    }

    @Override
    public String toString() {
        return "RandomSplitVariableProvider{" +
                "takeRate=" + this.takeRate +
                '}';
    }
}
