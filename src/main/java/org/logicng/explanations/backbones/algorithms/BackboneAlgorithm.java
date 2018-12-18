package org.logicng.explanations.backbones.algorithms;

import org.logicng.explanations.backbones.Backbone;
import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;

import java.util.Collection;

public interface BackboneAlgorithm {
    public Backbone computeBackbone(final SATSolver solver, final Collection<Variable> variables);
}
