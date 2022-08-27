package org.logicng.solvers.functions.splitvariablesprovider;

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
public class MostCommonVariablesProvider extends SplitVariableProviderWithTakeRate {

    /**
     * Creates a split variable provider returning the most common variables with a take rate of {@code 0.5}.
     */
    public MostCommonVariablesProvider() {
        super(0.5, 18);
    }

    /**
     * Creates a split variable provider returning the most common variables.
     * <p>
     * The take rate specifies the number of variables which should be returned in {@link #getSplitVars}.
     * So the result will contain {@code Math.min(maximumNumberOfVariables, (int) Math.ceil(variables.size() * takeRate))} variables.
     * @param takeRate                 the take rate, must be &gt; 0 and &lt;=1
     * @param maximumNumberOfVariables the maximum number of variables which should be selected
     */
    public MostCommonVariablesProvider(final double takeRate, final int maximumNumberOfVariables) {
        super(takeRate, maximumNumberOfVariables);
    }

    @Override
    public SortedSet<Variable> getSplitVars(final SATSolver solver, final Collection<Variable> variables) {
        return chooseVariablesByOccurrences(solver, variables, true);
    }
}
