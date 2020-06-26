package org.logicng.algorithms;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.propositions.Proposition;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.functions.OptimizationFunction;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Computation of a SMUS (smallest MUS, smallest minimal unsatisfiable set).
 * <p>
 * Implementation is based on &quot;Smallest MUS extraction with minimal
 * hitting set dualization&quot; (Ignatiev, Previti, Liffiton, &amp;
 * Marques-Silva, 2015).
 * @version 2.0.0
 * @since 2.0.0
 */
public final class SmusComputation {

    private static final String PROPOSITION_SELECTOR = "@PROPOSITION_SEL_";

    /**
     * Private empty constructor.  Class only contains static utility methods.
     */
    private SmusComputation() {
        // Intentionally left empty
    }

    /**
     * TODO: Adjust input and output?
     * Computes the SMUS for the given list of propositions modulo some additional constraint.
     * @param propositions          the propositions
     * @param additionalConstraints the additional constraints
     * @param f                     the formula factory
     * @param <P>                   the subtype of the propositions
     * @return the SMUS or {@code null} if the given propositions are satisfiable
     */
    public static <P extends Proposition> List<P> computeSmus(final List<P> propositions, final List<Formula> additionalConstraints, final FormulaFactory f) {
        final SATSolver growSolver = MiniSat.miniSat(f);
        growSolver.add(additionalConstraints == null ? Collections.singletonList(f.verum()) : additionalConstraints);
        final Map<Variable, P> propositionMapping = new TreeMap<>();
        for (final P proposition : propositions) {
            final Variable selector = f.variable(PROPOSITION_SELECTOR + propositionMapping.size());
            propositionMapping.put(selector, proposition);
            growSolver.add(f.equivalence(selector, proposition.formula()));
        }
        if (growSolver.sat(propositionMapping.keySet()) == Tristate.TRUE) {
            return null; // no MUS, since propositions are SAT
        }
        final SATSolver hSolver = MiniSat.miniSat(f);
        while (true) {
            final SortedSet<Variable> h = minimumHs(hSolver, propositionMapping.keySet());
            final SortedSet<Variable> c = grow(growSolver, h, propositionMapping.keySet());
            if (c == null) {
                return h.stream().map(propositionMapping::get).collect(Collectors.toList());
            }
            hSolver.add(f.or(c));
        }
    }

    /**
     * Computes the SMUS for the given list of formulas and some additional constraints.
     * @param formulas              the formulas
     * @param additionalConstraints the additional constraints
     * @param f                     the formula factory
     * @return the SMUS or {@code null} if the given formulas are satisfiable
     */
    public static List<Formula> computeSmusForFormulas(final List<Formula> formulas, final List<Formula> additionalConstraints, final FormulaFactory f) {
        final List<Proposition> props = formulas.stream().map(StandardProposition::new).collect(Collectors.toList());
        final List<Proposition> smus = computeSmus(props, additionalConstraints, f);
        return smus == null ? null : smus.stream().map(Proposition::formula).collect(Collectors.toList());
    }

    private static SortedSet<Variable> minimumHs(final SATSolver hSolver, final Set<Variable> variables) {
        return new TreeSet<>(hSolver.execute(OptimizationFunction.minimize(variables)).positiveVariables());
    }

    private static SortedSet<Variable> grow(final SATSolver growSolver, final SortedSet<Variable> h, final Set<Variable> variables) {
        final SolverState solverState = growSolver.saveState();
        growSolver.add(h);
        final Assignment maxModel = growSolver.execute(OptimizationFunction.maximize(variables));
        if (maxModel == null) {
            return null;
        }
        final List<Variable> maximumSatisfiableSet = maxModel.positiveVariables();
        growSolver.loadState(solverState);
        final SortedSet<Variable> minimumCorrectionSet = new TreeSet<>(variables);
        minimumCorrectionSet.removeAll(maximumSatisfiableSet);
        return minimumCorrectionSet;
    }
}
