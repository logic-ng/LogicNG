package org.logicng.solvers.functions.splitVariableProvider;

import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;

import java.util.Collection;
import java.util.SortedSet;

/**
 * An interface for split variable providers for model enumeration functions.
 * @version 2.4.0
 * @since 2.4.0
 */
public abstract class SplitVariableProvider {

    ///**
    // * Generates a set of split variables for a given formula. Such a set of split variables can then be used for the
    // * {@link ModelEnumerationFunction} and the {@link org.logicng.solvers.functions.AdvancedModelEnumerationFunction}.
    // * @param formulas the formulas
    // * @return the split variables
    // */
    public abstract SortedSet<Variable> getSplitVars(final SATSolver solver, final Collection<Variable> variables);
}
