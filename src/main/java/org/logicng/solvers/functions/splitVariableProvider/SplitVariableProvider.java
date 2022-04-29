package org.logicng.solvers.functions.splitVariableProvider;

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
 * @version 2.3.0
 * @since 2.3.0
 */
public abstract class SplitVariableProvider {
    int minNumberOfVars = 12;
    int lowerBound = 50;
    int upperBound = 65;
    final static double HUNDRED = 100;

    /**
     * Generates a set of split variables for a given formula. Such a set of split variables can then be used for the
     * {@link ModelEnumerationFunction} and the {@link org.logicng.solvers.functions.AdvancedModelEnumerationFunction}.
     * @param formulas the formulas
     * @return the split variables
     */
    public abstract SortedSet<Variable> getSplitVars(final Collection<Formula> formulas, final Collection<Variable> variables);

    boolean notWorthSplitting(final Collection<Variable> variables) {
        return variables.size() < this.minNumberOfVars;
    }

    Map<Integer, SortedSet<Variable>> getOccurrence2Vars(final Collection<Formula> formulas, final Collection<Variable> relevantVars) {
        final Formula formula;
        if (formulas.stream().findAny().isPresent()) {
            formula = formulas.stream().findAny().get().factory().and(formulas);
            if (formula.isConstantFormula()) {
                return null;
            }
        } else {
            return null;
        }
        final Map<Integer, SortedSet<Variable>> occurrence2Vars = new TreeMap<>();
        final Map<Variable, Integer> variableIntegerMap = formula.apply(new VariableProfileFunction()).entrySet().stream()
                .filter(x -> !isHelpVar(x.getKey()))
                .filter(x -> relevantVars.contains(x.getKey()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        for (final Map.Entry<Variable, Integer> entry : variableIntegerMap.entrySet()) {
            occurrence2Vars.computeIfAbsent(entry.getValue(), x -> new TreeSet<>()).add(entry.getKey());
        }
        return occurrence2Vars;
    }

    boolean isHelpVar(final Variable var) {
        return var.name().startsWith(CC_PREFIX) || var.name().startsWith(PB_PREFIX) || var.name().startsWith(CNF_PREFIX);
    }

    void fillSplitVars(final Map<Integer, SortedSet<Variable>> occurrence2Vars, final int counter, final int minNumberOfSplitVars,
                       final int maxNumberOfSplitVars, final SortedSet<Variable> splitVars) {
        if (occurrence2Vars.containsKey(counter)) {
            final List<Variable> vars = new ArrayList<>(occurrence2Vars.get(counter));
            if (vars.size() + splitVars.size() <= maxNumberOfSplitVars) {
                splitVars.addAll(vars);
            } else {
                final int diffToMin = minNumberOfSplitVars - splitVars.size();
                for (int i = 0; i <= diffToMin; i++) {
                    splitVars.add(vars.get(i));
                }
            }
        }
    }

    int getMinNumberOfSplitVars(final Collection<Variable> variables) {
        return (int) Math.ceil(this.lowerBound * variables.size() / HUNDRED);
    }

    int getMaxNumberOfSplitVars(final Collection<Variable> variables) {
        return (int) Math.floor(this.upperBound * variables.size() / HUNDRED);
    }
}
