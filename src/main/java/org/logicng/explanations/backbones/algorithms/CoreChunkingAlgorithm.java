package org.logicng.explanations.backbones.algorithms;

import org.logicng.datastructures.Tristate;
import org.logicng.explanations.backbones.Backbone;
import org.logicng.explanations.backbones.BackboneConfig;
import org.logicng.explanations.backbones.BackboneGeneration;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;

import java.util.*;

/**
 * An iterative Backbone algorithm that uses cores to distinguish Backbone literals and tests only single chunks of
 * literals in each iteration.
 * @version 1.5
 * @since 1.5
 */
public class CoreChunkingAlgorithm implements BackboneAlgorithm {

    public CoreChunkingAlgorithm() {}

    @Override
    public Backbone computeBackbone(SATSolver solver, Collection<Variable> variables) {
        solver.sat();
        SortedSet<Literal> implicant = solver.model(variables).literals();
        int chunksize = implicant.size(); // TODO find out which chunk size to use
        SolverState originalState = solver.saveState();
        Backbone backbone = new Backbone();

        while(!implicant.isEmpty()) {
            int k = Math.min(chunksize, implicant.size());
            final Iterator<Literal> it = implicant.iterator();
            SortedSet<Literal> chunk = new TreeSet<>();
            for(int i = 0; i < k; i++) {
                chunk.add(it.next());
            }
            SortedSet<Literal> flipped = new TreeSet<>();
            for (Literal l : chunk) {
                flipped.add(l.negate());
            }
            while (Boolean.TRUE) {
                SolverState before = solver.saveState();
                solver.add(flipped);
                if (solver.sat() == Tristate.TRUE) {
                    implicant.retainAll(solver.model(variables).literals());
                    solver.loadState(before);
                    break;
                }
                List<Proposition> core = solver.unsatCore().propositions();
                solver.loadState(before);
                SortedSet<Literal> coreLiterals = new TreeSet<>();
                for (Proposition coreProposition : core) {
                    coreLiterals.addAll(coreProposition.formulas().formula(solver.factory()).literals());
                }
                SortedSet<Literal> intersection = new TreeSet<>(coreLiterals);
                intersection.retainAll(flipped);
                if (intersection.size() == 1) {
                    Literal backboneLiteral = intersection.first();
                    backbone.add(backboneLiteral.negate());
                    implicant.remove(backboneLiteral.negate());
                    solver.add(backboneLiteral.negate());
                }
                flipped.removeAll(coreLiterals);
                if (flipped.isEmpty()) {
                    // TODO decide how to handle fall back algorithm and test only _remaining_ literals
                    final BackboneConfig config = new BackboneConfig.Builder().algorithm(BackboneConfig.Algorithm.ITERATIVE_PLAIN).build();
                    final BackboneGeneration backboneGeneration = new BackboneGeneration(config);
                    backbone.add(backboneGeneration.computeBackbone(solver, variables).getCompleteBackbone());
                    implicant.removeAll(chunk);
                    break;
                }
            }
        }
        solver.loadState(originalState);
        return backbone;
    }
}
