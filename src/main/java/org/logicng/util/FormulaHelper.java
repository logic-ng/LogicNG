package org.logicng.util;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Supplier;

/**
 * A class which contains utility methods for {@link Formula} objects.
 * @version 1.5.1
 * @since 1.5.1
 */
public final class FormulaHelper {

    /**
     * Private empty constructor.  Class only contains static utility methods.
     */
    private FormulaHelper() {
        // Intentionally left empty
    }

    /**
     * Returns all variables occurring in the given formulas.
     * @param formulas formulas
     * @return all variables occurring in the given formulas
     */
    public static SortedSet<Variable> variables(final Formula... formulas) {
        final SortedSet<Variable> variables = new TreeSet<>();
        for (final Formula f : formulas) {
            variables.addAll(f.variables());
        }
        return variables;
    }

    /**
     * Returns all variables occurring in the given formulas.
     * @param formulas formulas
     * @return all variables occurring in the given formulas
     */
    public static SortedSet<Variable> variables(final Collection<? extends Formula> formulas) {
        final SortedSet<Variable> variables = new TreeSet<>();
        for (final Formula f : formulas) {
            variables.addAll(f.variables());
        }
        return variables;
    }

    /**
     * Returns all literals occurring in the given formulas.
     * @param formulas formulas
     * @return all literals occurring in the given formulas
     */
    public static SortedSet<Literal> literals(final Formula... formulas) {
        final SortedSet<Literal> literals = new TreeSet<>();
        for (final Formula f : formulas) {
            literals.addAll(f.literals());
        }
        return literals;
    }

    /**
     * Returns all literals occurring in the given formulas.
     * @param formulas formulas
     * @return all literals occurring in the given formulas
     */
    public static SortedSet<Literal> literals(final Collection<? extends Formula> formulas) {
        final SortedSet<Literal> literals = new TreeSet<>();
        for (final Formula f : formulas) {
            literals.addAll(f.literals());
        }
        return literals;
    }

    /**
     * Returns the negation of the given literals
     * @param literals          the literals
     * @param collectionFactory the supplier for the collection
     * @param <C>               the type parameters of the collection
     * @return the negated literals
     */
    public static <C extends Collection<Literal>> C negateLiterals(final Collection<Literal> literals, final Supplier<C> collectionFactory) {
        final C result = collectionFactory.get();
        for (final Literal lit : literals) {
            result.add(lit.negate());
        }
        return result;
    }

    /**
     * Returns the negation of the given formulas.
     * @param formulas          the formulas
     * @param collectionFactory the supplier for the collection
     * @param <C>               the type parameters of the collection
     * @return the negated literals
     */
    public static <C extends Collection<Formula>> C negate(final Collection<Formula> formulas, final Supplier<C> collectionFactory) {
        final C result = collectionFactory.get();
        for (final Formula formula : formulas) {
            result.add(formula.negate());
        }
        return result;
    }

    /**
     * Converts a literal array to a variable array.
     * @param literals the literals
     * @return the variables
     */
    public static Variable[] literalsAsVariables(final Literal[] literals) {
        final Variable[] vars = new Variable[literals.length];
        for (int i = 0; i < vars.length; i++) {
            vars[i] = literals[i].variable();
        }
        return vars;
    }
}
