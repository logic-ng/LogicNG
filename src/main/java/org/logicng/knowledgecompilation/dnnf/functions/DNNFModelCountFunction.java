package org.logicng.knowledgecompilation.dnnf.functions;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.formulas.cache.FunctionCacheEntry;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A DNNF function which counts models.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class DNNFModelCountFunction implements DNNFFunction<BigInteger> {

    private static final DNNFModelCountFunction INSTANCE = new DNNFModelCountFunction();

    private DNNFModelCountFunction() {
        // intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static DNNFModelCountFunction get() {
        return INSTANCE;
    }

    @Override
    public BigInteger apply(final SortedSet<Variable> originalVariables, final Formula formula) {
        final Object cached = formula.functionCacheEntry(FunctionCacheEntry.DNNF_MODELCOUNT);
        final BigInteger result;
        if (cached != null) {
            result = (BigInteger) cached;
        } else {
            result = count(formula, new HashMap<>());
        }
        formula.setFunctionCacheEntry(FunctionCacheEntry.DNNF_MODELCOUNT, result);
        final SortedSet<Variable> dontCareVariables = new TreeSet<>();
        final SortedSet<Variable> dnnfVariables = formula.variables();
        for (final Variable originalVariable : originalVariables) {
            if (!dnnfVariables.contains(originalVariable)) {
                dontCareVariables.add(originalVariable);
            }
        }
        final BigInteger factor = BigInteger.valueOf(2).pow(dontCareVariables.size());
        return result.multiply(factor);
    }

    private BigInteger count(final Formula dnnf, final Map<Formula, BigInteger> internalCache) {
        BigInteger c = internalCache.get(dnnf);
        if (c == null) {
            switch (dnnf.type()) {
                case LITERAL:
                case TRUE:
                    c = BigInteger.ONE;
                    break;
                case AND:
                    c = BigInteger.ONE;
                    for (final Formula op : dnnf) {
                        c = c.multiply(count(op, internalCache));
                    }
                    break;
                case OR:
                    final int allVariables = dnnf.variables().size();
                    c = BigInteger.ZERO;
                    for (final Formula op : dnnf) {
                        final BigInteger opCount = count(op, internalCache);
                        final BigInteger factor = BigInteger.valueOf(2L).pow(allVariables - op.variables().size());
                        c = c.add(opCount.multiply(factor));
                    }
                    break;
                case FALSE:
                    c = BigInteger.ZERO;
                    break;
            }
            internalCache.put(dnnf, c);
        }
        return c;
    }
}
