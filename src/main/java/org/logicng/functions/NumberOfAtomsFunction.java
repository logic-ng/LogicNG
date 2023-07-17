package org.logicng.functions;

import static org.logicng.formulas.cache.FunctionCacheEntry.NUMBER_OF_ATOMS;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFunction;
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;

/**
 * A function that computes the number of atoms occurring in a given formula.
 * @version 2.2.0
 * @since 2.2.0
 */
public class NumberOfAtomsFunction implements FormulaFunction<Long> {

    private final static NumberOfAtomsFunction INSTANCE = new NumberOfAtomsFunction();

    /**
     * Private empty constructor. Singleton class.
     */
    private NumberOfAtomsFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the function.
     * @return the function instance
     */
    public static NumberOfAtomsFunction get() {
        return INSTANCE;
    }

    @Override
    public Long apply(final Formula formula, final boolean cache) {
        final Object cached = formula.functionCacheEntry(NUMBER_OF_ATOMS);
        if (cached != null) {
            return (Long) cached;
        }
        long result = 0L;
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
            case PBC:
                result = 1L;
                break;
            case NOT:
                result = apply(((Not) formula).operand(), cache);
                break;
            case IMPL:
            case EQUIV:
                final BinaryOperator binary = (BinaryOperator) formula;
                result = apply(binary.left(), cache) + apply(binary.right(), cache);
                break;
            case OR:
            case AND:
                final NAryOperator nary = (NAryOperator) formula;
                for (final Formula op : nary) {
                    result += apply(op, cache);
                }
                break;
            default:
                throw new IllegalStateException("Unknown formula type " + formula.type());
        }
        if (cache) {
            formula.setFunctionCacheEntry(NUMBER_OF_ATOMS, result);
        }
        return result;
    }
}
