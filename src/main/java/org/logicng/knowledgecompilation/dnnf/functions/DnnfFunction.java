// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.dnnf.functions;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.SortedSet;

/**
 * A function which can be applied on a DNNF.
 * @param <RESULT> the result type of the function
 * @version 2.0.0
 * @since 2.0.0
 */
public interface DnnfFunction<RESULT> {

    /**
     * Applies this function to a given DNNF
     * @param originalVariables the original variables of the DNNF
     * @param formula           the formula of the DNNF
     * @return the result of the function application
     */
    RESULT apply(final SortedSet<Variable> originalVariables, final Formula formula);
}
