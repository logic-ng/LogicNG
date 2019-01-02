package org.logicng.explanations.backbones.algorithms;

import org.logicng.datastructures.Tristate;
import org.logicng.explanations.backbones.Backbone;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;

/**
 * An iterative Backbone algorithm that iteratively refines an implicant of a formula until it contains only Backbone
 * literals.
 * @version 1.5
 * @since 1.5
 */
public class IterativePlusAlgorithm implements BackboneAlgorithm {

    public IterativePlusAlgorithm() {}

    @Override
    public Backbone computeBackbone(SATSolver solver, Collection<Variable> variables) {
        solver.sat();
        Collection<Literal> backboneLiterals = solver.model(variables).literals();
        FormulaFactory f = solver.factory();
        
        while(!backboneLiterals.isEmpty()) {
            SolverState before = solver.saveState();
            List<Formula> complement = new ArrayList<>();
            for(Literal l : backboneLiterals) {
                complement.add(l.negate());
            }
            solver.add(f.or(complement));
            if(solver.sat() == Tristate.FALSE) {
                break;
            }
            backboneLiterals.retainAll(solver.model(variables).literals());
            solver.loadState(before);
        }

        Backbone backbone = new Backbone();
        backbone.add(backboneLiterals);
        return backbone;
    }
}
