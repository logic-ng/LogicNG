package org.logicng.explanations.backbones;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.Collection;
import java.util.Collections;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Convenient methods for backbone computation.
 *
 * Note: For more control over the backbone solver create an instance of {@link MiniSatBackbone} directly.
 * E.g., with an instance of {@link MiniSatBackbone} the already loaded formulas can be re-used for multiple
 * backbone computations.
 * @version 1.5
 * @since 1.5
 */
public class BackboneGeneration {

    private static final MiniSatBackbone solver = new MiniSatBackbone();

    /**
     * Private constructor.
     */
    private BackboneGeneration() {
        // Intentionally left empty.
    }

    private static SortedSet<Variable> variables(final Collection<Formula> formulas) {
        final SortedSet<Variable> variables = new TreeSet<>();
        for (final Formula formula : formulas) {
            variables.addAll(formula.variables());
        }
        return variables;
    }

    public Backbone compute(final Collection<Formula> formulas, final Collection<Variable> variables, final BackboneType type) {
        solver.reset();
        solver.add(formulas);
        return solver.compute(variables, type);
    }

    public Backbone compute(final Collection<Formula> formulas, final Collection<Variable> variables) {
        return compute(formulas, variables, BackboneType.POSITIVE_AND_NEGATIVE);
    }

    public Backbone compute(final Collection<Formula> formulas, final BackboneType type) {
        return compute(formulas, variables(formulas), type);
    }

    public Backbone compute(final Collection<Formula> formulas) {
        return compute(formulas, variables(formulas), BackboneType.POSITIVE_AND_NEGATIVE);
    }

    public Backbone compute(final Formula formula, final Collection<Variable> variables, final BackboneType type) {
        return compute(Collections.singletonList(formula), variables, type);
    }

    public Backbone compute(final Formula formula, final Collection<Variable> variables) {
        return compute(formula, variables, BackboneType.POSITIVE_AND_NEGATIVE);
    }

    public Backbone compute(final Formula formula, final BackboneType type) {
        return compute(formula, formula.variables(), type);
    }

    public Backbone compute(final Formula formula) {
        return compute(formula, formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE);
    }
}
