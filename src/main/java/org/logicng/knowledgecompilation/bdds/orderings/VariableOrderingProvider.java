// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.orderings;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;

import java.util.List;

/**
 * An interface for variable ordering providers for BDDs.
 * @version 1.4.0
 * @since 1.4.0
 */
public interface VariableOrderingProvider {

    /**
     * Generates a variable ordering for a given formula.  Such a variable ordering can then be
     * used for the initialization of the BDD Kernel in {@link org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel#BDDKernel(FormulaFactory, List, int, int)}.
     * @param formula the formula
     * @return the variable ordering
     */
    List<Variable> getOrder(final Formula formula);
}
