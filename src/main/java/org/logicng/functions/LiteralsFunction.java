package org.logicng.functions;

import static org.logicng.formulas.cache.FunctionCacheEntry.LITERALS;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFunction;
import org.logicng.formulas.Literal;
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;
import org.logicng.util.FormulaHelper;

import java.util.Collections;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A function that computes all literals occurring in a given formula.
 * @version 2.2.0
 * @since 2.2.0
 */
public class LiteralsFunction implements FormulaFunction<SortedSet<Literal>> {

    private final static LiteralsFunction INSTANCE = new LiteralsFunction();

    /**
     * Private empty constructor. Singleton class.
     */
    private LiteralsFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the function.
     * @return the function instance
     */
    public static LiteralsFunction get() {
        return INSTANCE;
    }

    @Override
    public SortedSet<Literal> apply(final Formula formula, final boolean cache) {
        final Object cached = formula.functionCacheEntry(LITERALS);
        if (cached != null) {
            return (SortedSet<Literal>) cached;
        }
        SortedSet<Literal> result = new TreeSet<>();
        switch (formula.type()) {
            case FALSE:
            case TRUE:
                result = new TreeSet<>();
                break;
            case LITERAL:
                final Literal lit = (Literal) formula;
                result.add(lit);
                break;
            case NOT:
                final Not not = (Not) formula;
                result = apply(not.operand(), cache);
                break;
            case IMPL:
            case EQUIV:
                final BinaryOperator binary = (BinaryOperator) formula;
                result.addAll(apply(binary.left(), cache));
                result.addAll(apply(binary.right(), cache));
                break;
            case OR:
            case AND:
                final NAryOperator nary = (NAryOperator) formula;
                for (final Formula op : nary) {
                    result.addAll(apply(op, cache));
                }
                break;
            case PBC:
                final PBConstraint pbc = (PBConstraint) formula;
                result = FormulaHelper.literals(pbc.operands());
                break;
            default:
                throw new IllegalStateException("Unknown formula type " + formula.type());
        }
        result = Collections.unmodifiableSortedSet(result);
        if (cache) {
            formula.setFunctionCacheEntry(LITERALS, result);
        }
        return result;
    }
}
