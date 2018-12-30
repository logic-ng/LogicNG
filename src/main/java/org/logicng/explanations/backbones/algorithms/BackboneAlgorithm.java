package org.logicng.explanations.backbones.algorithms;

import org.logicng.explanations.backbones.Backbone;
import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;

import java.util.Collection;

/**
 * Interface for the Backbone computation algorithms.
 * @version 1.5
 * @since 1.5
 */
public interface BackboneAlgorithm {

    /**
     * Computes a Backbone for the formulas on the provided SAT solver under consideration of only the given variables.
     * @param solver    SAT solver that has added the formulas for which the Backbone should be computed
     * @param variables Variables of the formulas that need to be considered for the Backbone computation
     * @return  the Backbone
     */
    public Backbone computeBackbone(final SATSolver solver, final Collection<Variable> variables);
}
