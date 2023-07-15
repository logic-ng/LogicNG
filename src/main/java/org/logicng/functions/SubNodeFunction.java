// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.functions;

import static org.logicng.formulas.cache.FunctionCacheEntry.SUBFORMULAS;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFunction;

import java.util.LinkedHashSet;

/**
 * A function that computes all sub-nodes of a given formula.  The order of the sub-nodes is bottom-up, i.e. a
 * sub-node only appears in the result when all of its sub-nodes are already listed.
 * @version 2.3.0
 * @since 1.0
 */
public final class SubNodeFunction implements FormulaFunction<LinkedHashSet<Formula>> {

    private static final SubNodeFunction INSTANCE = new SubNodeFunction();

    /**
     * @deprecated In the next version, the standard constructor will be replaced by a private constructor.
     * In order to instantiate an object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public SubNodeFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static SubNodeFunction get() {
        return INSTANCE;
    }

    @Override
    @SuppressWarnings("unchecked")
    public LinkedHashSet<Formula> apply(final Formula formula, final boolean cache) {
        final Object cached = formula.functionCacheEntry(SUBFORMULAS);
        if (cached != null) {
            return (LinkedHashSet<Formula>) cached;
        }
        final LinkedHashSet<Formula> result = new LinkedHashSet<>();
        for (final Formula op : formula) {
            if (!result.contains(op)) {
                result.addAll(apply(op, cache));
            }
        }
        result.add(formula);
        if (cache) {
            formula.setFunctionCacheEntry(SUBFORMULAS, result);
        }
        return result;
    }
}
