package org.logicng.explanations.backbones.algorithms;

import org.logicng.datastructures.Tristate;
import org.logicng.explanations.backbones.Backbone;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;

import java.util.Collection;
import java.util.SortedSet;

/**
 * An iterative Backbone algorithm that tests one literal in each iteration.
 * @version 1.5
 * @since 1.5
 */
public class IterativeOneTestAlgorithm implements BackboneAlgorithm {

    public IterativeOneTestAlgorithm() {}

    @Override
    public Backbone computeBackbone(SATSolver solver, Collection<Variable> variables) {
        Backbone backbone = new Backbone();
        solver.sat(); 
        SortedSet<Literal> implicant = solver.model(variables).literals();

        while(!implicant.isEmpty()) {
            Literal l = implicant.first();
            Tristate sat = solver.sat(l.negate());
            if (sat == Tristate.FALSE) {
                backbone.add(l);
                implicant.remove(l);
                solver.add(l);
            } else {
                implicant.retainAll(solver.model(variables).literals());
            }
        }

        return backbone;
    }
}
