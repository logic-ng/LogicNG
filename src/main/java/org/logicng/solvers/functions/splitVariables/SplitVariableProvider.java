package org.logicng.solvers.functions.splitVariables;

import static org.logicng.formulas.FormulaFactory.CC_PREFIX;
import static org.logicng.formulas.FormulaFactory.CNF_PREFIX;
import static org.logicng.formulas.FormulaFactory.PB_PREFIX;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.functions.VariableProfileFunction;
import org.logicng.solvers.functions.ModelEnumerationFunction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * An interface for split variable providers for model enumeration functions.
 * @version 2.2.1
 * @since 2.2.1
 */
public interface SplitVariableProvider {
    double HUNDRED = 100;

    /**
     * Generates a set of split variables for a given formula.  Such a set of split variables can then be used for the
     * {@link ModelEnumerationFunction}.
     * @param formulas the formulas
     * @return the split variables
     */
    SortedSet<Variable> getOrder(final Collection<Formula> formulas, final Collection<Variable> variables);

    default Map<Integer, SortedSet<Variable>> getOccurrence2Vars(final Collection<Formula> formulas) {
        final Formula formula = formulas.stream().findAny().get().factory().and(formulas);
        if (formula.isConstantFormula()) {
            return null;
        }
        final Map<Integer, SortedSet<Variable>> occurrence2Vars = new TreeMap<>();
        final Map<Variable, Integer> variableIntegerMap = formula.apply(new VariableProfileFunction()).entrySet().stream()
                .filter(x -> isNotHelpVar(x.getKey()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        for (final Map.Entry<Variable, Integer> entry : variableIntegerMap.entrySet()) {
            occurrence2Vars.computeIfAbsent(entry.getValue(), x -> new TreeSet<>()).add(entry.getKey());
        }
        return occurrence2Vars;
    }

    default boolean isNotHelpVar(final Variable var) {
        return !var.name().startsWith(CC_PREFIX) && !var.name().startsWith(PB_PREFIX) && !var.name().startsWith(CNF_PREFIX);
    }

    default void fillSplitVars(final Map<Integer, SortedSet<Variable>> occurrence2Vars, final int counter, final int minNumberOfSplitVars,
                               final int maxNumberOfSplitVars, final SortedSet<Variable> splitVars) {
        if (occurrence2Vars.containsKey(counter)) {
            if (occurrence2Vars.get(counter).size() + splitVars.size() <= maxNumberOfSplitVars) {
                splitVars.addAll(occurrence2Vars.get(counter));
            } else {
                final int diffToMin = minNumberOfSplitVars - splitVars.size();
                final List<Variable> variablesInMap = new ArrayList<>(occurrence2Vars.get(counter));
                for (int i = 0; i <= diffToMin; i++) {
                    splitVars.add(variablesInMap.get(i));
                }
            }
        }
    }
}
