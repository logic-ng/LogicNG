package org.logicng.solvers.functions.splitvariablesprovider;

import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.ModelEnumerationFunction;

import java.util.Collection;
import java.util.SortedSet;

/**
 * An interface for split variable providers for model enumeration functions.
 * @version 2.4.0
 * @since 2.4.0
 */
public abstract class SplitVariableProvider {

    /**
     * Generates a set of split variables for a given formula.
     * <p>
     * Such a set of split variables can then be used for the {@link ModelEnumerationFunction}.
     * @param solver    the solver
     * @param variables the variables from which the split variables should be chosen
     * @return the split variables
     */
    public abstract SortedSet<Variable> getSplitVars(final SATSolver solver, final Collection<Variable> variables);
}
