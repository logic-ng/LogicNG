package org.logicng.explanations.backbones.algorithms;

import jdk.nashorn.internal.objects.annotations.Function;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.backbones.Backbone;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;

import java.util.*;

/**
 * An iterative Backbone algorithm that tests multiple literals (called a chunk) in each iteration.
 * @version 1.5
 * @since 1.5
 */
public class ChunkingAlgorithm implements BackboneAlgorithm {

    public ChunkingAlgorithm() {}

    @Override
    public Backbone computeBackbone(SATSolver solver, Collection<Variable> variables) {
        FormulaFactory f = solver.factory();
        solver.sat();
        SortedSet<Literal> implicant = solver.model(variables).literals();
        int chunksize = implicant.size(); // TODO find out which chunk size to use
        Backbone backbone = new Backbone();

        while(!implicant.isEmpty()) {
            SolverState before = solver.saveState();
            int k = Math.min(chunksize, implicant.size());
            final Iterator<Literal> it = implicant.iterator();
            SortedSet<Literal> chunkSet = new TreeSet<>();
            for(int i = 0; i < k; i++) {
                chunkSet.add(it.next().negate());
            }
            Formula chunk = f.or(chunkSet);
            solver.add(chunk);
            if(solver.sat() == Tristate.FALSE) {
                SortedSet<Literal> negatedChunkSet = new TreeSet<>();
                for (Literal l : chunkSet) {
                    negatedChunkSet.add(l.negate());
                }
                backbone.add(negatedChunkSet);
                implicant.removeAll(negatedChunkSet);
                solver.loadState(before);
                solver.add(f.or(negatedChunkSet));
            } else {
                implicant.retainAll(solver.model(variables).literals());
                solver.loadState(before);
            }
        }

        return backbone;
    }
}
