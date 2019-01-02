package org.logicng.explanations.backbones.algorithms;

import org.logicng.datastructures.Tristate;
import org.logicng.explanations.backbones.Backbone;
import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;

import java.util.Collection;

/**
 * A plain iterative Backbone algorithm.
 * @version 1.5
 * @since 1.5
 */
public class IterativePlainAlgorithm implements BackboneAlgorithm {

    public IterativePlainAlgorithm() {}

    @Override
    public Backbone computeBackbone(SATSolver solver, Collection<Variable> variables) {
        Backbone backbone = new Backbone();
        for (Variable v : variables) {
            Tristate posTest = solver.sat(v);
            Tristate negTest = solver.sat(v.negate());
            if (posTest == Tristate.FALSE && negTest == Tristate.FALSE) {
                return new Backbone();
            }
            if (posTest == Tristate.FALSE) {
                backbone.add(v.negate());
                solver.add(v.negate());
            }
            if (negTest == Tristate.FALSE) {
                backbone.add(v);
                solver.add(v);
            }
        }
        return backbone;
    }
}
