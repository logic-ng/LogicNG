// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.util;

import static org.logicng.util.CollectionHelper.nullOrEmpty;

import org.logicng.formulas.And;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.Supplier;

/**
 * A class which contains utility methods for {@link Formula} objects.
 * @version 2.2.0
 * @since 1.5.1
 */
public final class FormulaHelper {

    /**
     * Private empty constructor. Class only contains static utility methods.
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
    public static <C extends Collection<Literal>> C negateLiterals(final Collection<? extends Literal> literals,
                                                                   final Supplier<C> collectionFactory) {
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
    public static <C extends Collection<Formula>> C negate(final Collection<? extends Formula> formulas,
                                                           final Supplier<C> collectionFactory) {
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

    /**
     * Splits the top level {@link And} if possible.
     * @param formula the formula
     * @return list of the operands if the formula is a {@link And}, otherwise
     *         the formula itself
     */
    public static List<Formula> splitTopLevelAnd(final Formula formula) {
        if (formula.type() == FType.AND) {
            final List<Formula> ops = new ArrayList<>();
            for (final Formula op : formula) {
                ops.add(op);
            }
            return ops;
        } else {
            return Collections.singletonList(formula);
        }
    }

    /**
     * Returns a sorted variable set from a given collection of strings which
     * will be used as variable names and a formula factory. The given
     * collection is treated in a null-safe manner, i.e. if the collection is
     * {@code null} the collection is considered to be an empty collection.
     * @param strings the collection of strings (can be empty or {@code null}
     * @param f       the formula factory which is used to generate the
     *                variables
     * @return the sorted set of variables from the collection of variable names
     */
    public static SortedSet<Variable> strings2vars(final Collection<String> strings, final FormulaFactory f) {
        if (nullOrEmpty(strings)) {
            return Collections.emptySortedSet();
        }
        final SortedSet<Variable> vars = new TreeSet<>();
        for (final String string : strings) {
            vars.add(f.variable(string));
        }
        return vars;
    }

    /**
     * Returns a sorted literal set from a given collection of strings and a
     * formula factory. If a string begins with the given {@code negationPrefix}
     * a literal with a negative phase is created, otherwise a literal with a
     * positive phase is created. The given collection is treated in a null-safe
     * manner, i.e. if the collection is {@code null} the collection is
     * considered to be an empty collection.
     * @param strings        the collection of strings (can be empty or
     *                       {@code null}
     * @param negationPrefix the negation prefix
     * @param f              the formula factory which is used to generate the
     *                       variables
     * @return the sorted set of literals
     */
    public static SortedSet<Literal> strings2literals(final Collection<String> strings, final String negationPrefix,
                                                      final FormulaFactory f) {
        if (nullOrEmpty(strings)) {
            return Collections.emptySortedSet();
        }
        final SortedSet<Literal> literals = new TreeSet<>();
        for (final String string : strings) {
            final boolean isPositive = !string.startsWith(negationPrefix);
            literals.add(f.literal(isPositive ? string : string.substring(negationPrefix.length()), isPositive));
        }
        return literals;
    }

    /**
     * Returns a list of strings from a given collection of variables. The
     * strings contain the variable names. The given collection is treated in a
     * null-safe manner, i.e. if the collection is {@code null} the collection
     * is considered to be an empty collection.
     * @param variables the collection of variables (can be empty or
     *                  {@code null}
     * @return the list of variable names
     */
    public static SortedSet<String> vars2strings(final Collection<Variable> variables) {
        if (nullOrEmpty(variables)) {
            return Collections.emptySortedSet();
        }
        final SortedSet<String> strings = new TreeSet<>();
        for (final Variable variable : variables) {
            strings.add(variable.name());
        }
        return strings;
    }

    /**
     * Returns a list of strings from a given collection of literals. The
     * strings contain the variable names with a leading {@code negationPrefix}
     * if the literal has a negative phase. The given collection is treated in a
     * null-safe manner, i.e. if the collection is {@code null} the collection
     * is considered to be an empty collection.
     * @param literals       the collection of variables (can be empty or
     *                       {@code null}
     * @param negationPrefix the negation prefix
     * @return the list of literal names with a leading {@code negationPrefix}
     *         if the phase is negative
     */
    public static SortedSet<String> literals2strings(final Collection<Literal> literals, final String negationPrefix) {
        if (nullOrEmpty(literals)) {
            return Collections.emptySortedSet();
        }
        final SortedSet<String> strings = new TreeSet<>();
        for (final Literal lit : literals) {
            strings.add(lit.phase() ? lit.name() : negationPrefix + lit.name());
        }
        return strings;
    }
}
