package org.logicng.explanations.backbones.algorithms;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.backbones.Backbone;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;

import java.util.Collection;
import java.util.TreeSet;

/**
 * A naive enumeration-based Backbone algorithm.
 * @version 1.5
 * @since 1.5
 */
public class EnumerationAlgorithm implements BackboneAlgorithm {

    public EnumerationAlgorithm() {}

    @Override
    public Backbone computeBackbone(SATSolver solver, Collection<Variable> variables) {
        Collection<Literal> literals = new TreeSet<>();
        for (Variable variable : variables) {
            literals.add(variable);
            literals.add(variable.negate());
        }

        while ((!literals.isEmpty()) && solver.sat() == Tristate.TRUE) {
            Assignment model = solver.model();
            literals.retainAll(model.literals());
            solver.add(model.blockingClause(solver.factory(), null));
        }

        Backbone backbone = new Backbone();
        backbone.add(literals);
        return backbone;
    }
}
