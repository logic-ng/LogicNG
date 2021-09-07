package org.logicng.functions;

import static org.logicng.formulas.cache.FunctionCacheEntry.NUMBER_OF_NODES;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFunction;
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;

/**
 * A function that computes the number of nodes of a given formula.
 * @version 2.2.0
 * @since 2.2.0
 */
public class NumberOfNodesFunction implements FormulaFunction<Long> {

    private final static NumberOfNodesFunction INSTANCE = new NumberOfNodesFunction();

    /**
     * Private empty constructor.  Singleton class.
     */
    private NumberOfNodesFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the function.
     * @return the function instance
     */
    public static NumberOfNodesFunction get() {
        return INSTANCE;
    }

    @Override
    public Long apply(final Formula formula, final boolean cache) {
        final Object cached = formula.functionCacheEntry(NUMBER_OF_NODES);
        if (cached != null) {
            return (Long) cached;
        }
        long result;
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                result = 1L;
                break;
            case NOT:
                result = apply(((Not) formula).operand(), cache) + 1L;
                break;
            case IMPL:
            case EQUIV:
                final BinaryOperator binary = (BinaryOperator) formula;
                result = apply(binary.left(), cache) + apply(binary.right(), cache) + 1L;
                break;
            case OR:
            case AND:
                final NAryOperator nary = (NAryOperator) formula;
                result = 1L;
                for (final Formula op : nary) {
                    result += apply(op, cache);
                }
                break;
            case PBC:
                final PBConstraint pbc = (PBConstraint) formula;
                result = 1L + pbc.operands().length;
                break;
            default:
                throw new IllegalStateException("Unknown formula type " + formula.type());
        }
        if (cache) {
            formula.setFunctionCacheEntry(NUMBER_OF_NODES, result);
        }
        return result;
    }
}
