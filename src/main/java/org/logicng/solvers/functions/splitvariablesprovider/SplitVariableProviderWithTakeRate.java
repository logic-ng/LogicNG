package org.logicng.solvers.functions.splitvariablesprovider;

import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.VariableOccurrencesOnSolverFunction;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Super class for variable providers which always return a subset of the given variables.
 * The number of selected variables is defined by the {@link #takeRate} which is the ration
 * (between 0 and 1) of selected variables.
 * @version 2.4.0
 * @since 2.4.0
 */
public abstract class SplitVariableProviderWithTakeRate extends SplitVariableProvider {
    protected final double takeRate;

    /**
     * Creates a new split variable provider with the given take rate.
     * @param takeRate the take rate, must be between 0 and 1 (each inclusive)
     */
    protected SplitVariableProviderWithTakeRate(final double takeRate) {
        if (takeRate < 0 || takeRate > 1) {
            throw new IllegalArgumentException("Take rate must be a value between 0 and 1");
        }
        this.takeRate = takeRate;
    }

    /**
     * Returns a subset of the most or least common variables. The number of returned variables is
     * defined by the take rate (see {@link #numberOfVariablesToChoose}).
     * @param solver     the solver used to count the variable occurrences
     * @param variables  the variables to choose from
     * @param mostCommon {@code true} is the most common variables should be selected, {@code false}
     *                   if the least common variables should be selected
     * @return a subset of the most or least common variables
     */
    protected SortedSet<Variable> chooseVariablesByOccurrences(final SATSolver solver, final Collection<Variable> variables, final boolean mostCommon) {
        final Comparator<Map.Entry<Variable, Integer>> comparator = mostCommon
                ? Map.Entry.comparingByValue(Comparator.reverseOrder())
                : Map.Entry.comparingByValue();
        final Set<Variable> vars = new HashSet<>(variables);
        final Map<Variable, Integer> variableOccurrences = solver.execute(new VariableOccurrencesOnSolverFunction(vars));
        return variableOccurrences.entrySet().stream()
                .sorted(comparator)
                .limit(numberOfVariablesToChoose(vars))
                .map(Map.Entry::getKey)
                .collect(Collectors.toCollection(TreeSet::new));
    }

    /**
     * Returns the number of variables which should be chosen. This depends on the number of variables
     * and the {@link #takeRate}.
     * @param variables the variables
     * @return the number of variables which should be chosen
     */
    protected int numberOfVariablesToChoose(final Collection<Variable> variables) {
        return (int) (variables.size() * this.takeRate);
    }
}
