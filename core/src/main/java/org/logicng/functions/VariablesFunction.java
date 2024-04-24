package org.logicng.functions;

import static org.logicng.formulas.cache.FunctionCacheEntry.VARIABLES;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFunction;
import org.logicng.formulas.Literal;
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.util.FormulaHelper;

import java.util.Collections;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A function that computes all variables occurring in a given formula.
 * @version 2.2.0
 * @since 2.2.0
 */
public class VariablesFunction implements FormulaFunction<SortedSet<Variable>> {

    private final static VariablesFunction INSTANCE = new VariablesFunction();

    /**
     * Private empty constructor. Singleton class.
     */
    private VariablesFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the function.
     * @return the function instance
     */
    public static VariablesFunction get() {
        return INSTANCE;
    }

    @Override
    public SortedSet<Variable> apply(final Formula formula, final boolean cache) {
        final Object cached = formula.functionCacheEntry(VARIABLES);
        if (cached != null) {
            return (SortedSet<Variable>) cached;
        }
        SortedSet<Variable> result = new TreeSet<>();
        switch (formula.type()) {
            case FALSE:
            case TRUE:
                result = new TreeSet<>();
                break;
            case LITERAL:
                final Literal lit = (Literal) formula;
                result.add(lit.variable());
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
                result = FormulaHelper.variables(pbc.literals());
                break;
            default:
                throw new IllegalStateException("Unknown formula type " + formula.type());
        }
        result = Collections.unmodifiableSortedSet(result);
        if (cache) {
            formula.setFunctionCacheEntry(VARIABLES, result);
        }
        return result;
    }
}
