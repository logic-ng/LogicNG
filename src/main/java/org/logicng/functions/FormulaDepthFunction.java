package org.logicng.functions;

import static org.logicng.formulas.cache.FunctionCacheEntry.DEPTH;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFunction;

/**
 * A function that computes the depth of a formula. The depth of an atomic formula
 * is defined as 0, all other operators increase the depth by 1.
 * @version 2.0
 * @since 2.0
 */
public final class FormulaDepthFunction implements FormulaFunction<Integer> {

    @Override
    public Integer apply(final Formula formula, final boolean cache) {
        final Object cached = formula.functionCacheEntry(DEPTH);
        if (cached != null) {
            return (Integer) cached;
        }
        final int result;
        if (formula.isAtomicFormula()) {
            result = 0;
        } else {
            int maxDepth = 0;
            for (final Formula op : formula) {
                maxDepth = Math.max(maxDepth, apply(op, cache));
            }
            result = maxDepth + 1;
        }
        if (cache) {
            formula.setFunctionCacheEntry(DEPTH, result);
        }
        return result;
    }
}
