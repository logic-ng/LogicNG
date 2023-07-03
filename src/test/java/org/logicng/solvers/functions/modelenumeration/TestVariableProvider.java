package org.logicng.solvers.functions.modelenumeration;

import static java.util.Collections.emptySortedSet;

import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.modelenumeration.splitvariablesprovider.SplitVariableProvider;

import java.util.Collection;
import java.util.SortedSet;

public class TestVariableProvider {
    public static class EmptyVariableProvider implements SplitVariableProvider {
        @Override
        public SortedSet<Variable> getSplitVars(final SATSolver solver, final Collection<Variable> variables) {
            return emptySortedSet();
        }
    }

    public static class NullVariableProvider implements SplitVariableProvider {
        @Override
        public SortedSet<Variable> getSplitVars(final SATSolver solver, final Collection<Variable> variables) {
            return null;
        }
    }
}


