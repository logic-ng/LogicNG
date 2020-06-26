package org.logicng.knowledgecompilation.dnnf.datastructures.dtree;

import org.logicng.formulas.Formula;

/**
 * A generator for a DTree.
 * @version 2.0.0
 * @since 2.0.0
 */
public interface DTreeGenerator {

    /**
     * Generates a DTree for the given CNF.
     * @param cnf the CNF
     * @return the DTree
     */
    DTree generate(final Formula cnf);
}
