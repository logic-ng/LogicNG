package org.logicng.solvers.functions.splitVariableProvider;

import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;

import java.util.Collection;
import java.util.SortedSet;

/**
 * A split variable provider which provides split variables which occur particularly often in the formulas on the solver. The variables occurring in the
 * formulas are sorted by their occurrence. This provider returns those variables with the biggest occurrence.
 * @version 2.4.0
 * @since 2.4.0
 */
public class MostCommonVariableProvider extends SplitVariableProviderWithTakeRate {

    /**
     * Creates a split variable provider returning the most common variables.
     * <p>
     * The take rate specifies the number of variables which should be returned in {@link #getSplitVars}.
     * So the result will contain {@code (int) (variables.size() * takeRate)} variables.
     * @param takeRate the take rate, must be between 0 and 1 (each inclusive)
     */
    public MostCommonVariableProvider(final double takeRate) {
        super(takeRate);
    }

    @Override
    public SortedSet<Variable> getSplitVars(final SATSolver solver, final Collection<Variable> variables) {
        return chooseVariablesByOccurrences(solver, variables, true);
    }

}
