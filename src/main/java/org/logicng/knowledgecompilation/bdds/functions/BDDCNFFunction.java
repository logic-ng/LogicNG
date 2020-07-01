package org.logicng.knowledgecompilation.bdds.functions;

import org.logicng.formulas.Formula;
import org.logicng.knowledgecompilation.bdds.datastructures.BDD;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

import java.util.ArrayList;
import java.util.List;

/**
 * Creates a CNF from a BDD.
 * @version 2.0.0
 * @since 2.0.0
 */
public class BDDCNFFunction implements BDDFunction<Formula> {

    @Override
    public Formula apply(final BDD bdd) {
        final BDDKernel kernel = bdd.underlyingKernel();
        final List<byte[]> unsatPaths = kernel.allUnsat(bdd.index());
        final List<Formula> clauses = new ArrayList<>();
        List<Formula> literals;
        for (final byte[] path : unsatPaths) {
            literals = new ArrayList<>();
            for (int i = 0; i < path.length; i++) {
                if (path[i] == 0) {
                    literals.add(kernel.idx2var().get(i));
                } else if (path[i] == 1) {
                    literals.add(kernel.idx2var().get(i).negate());
                }
            }
            clauses.add(kernel.factory().or(literals));
        }
        return kernel.factory().and(clauses);
    }
}