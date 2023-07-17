// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.backbones;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.handlers.SATHandler;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.functions.BackboneFunction;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.util.FormulaHelper;

import java.util.Collection;
import java.util.Collections;

/**
 * Main entry point for backbone computations.
 * <p>
 * This class provides convenient methods for backbone computation for many use
 * cases.
 * @version 2.1.0
 * @since 1.5.0
 */
public final class BackboneGeneration {

    /**
     * Private constructor.
     */
    private BackboneGeneration() {
        // Intentionally left empty.
    }

    /**
     * Computes the backbone for a given collection of formulas w.r.t. a
     * collection of variables and a backbone type.
     * @param formulas  the given collection of formulas
     * @param variables the given collection of relevant variables for the
     *                  backbone computation
     * @param type      the type of backbone variables that should be computed
     * @param handler   an optional handler for the backbone computation's SAT
     *                  solver
     * @return the backbone or {@code null} if the computation was aborted by
     *         the handler
     */
    public static Backbone compute(final Collection<Formula> formulas, final Collection<Variable> variables,
                                   final BackboneType type, final SATHandler handler) {
        if (formulas == null || formulas.isEmpty()) {
            throw new IllegalArgumentException("Provide at least one formula for backbone computation");
        }
        final FormulaFactory f = formulas.iterator().next().factory();
        final MiniSat miniSat =
                MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        miniSat.add(formulas);
        return miniSat.execute(BackboneFunction.builder().handler(handler).variables(variables).type(type).build());
    }

    /**
     * Computes the backbone for a given collection of formulas w.r.t. a
     * collection of variables and a backbone type.
     * @param formulas  the given collection of formulas
     * @param variables the given collection of relevant variables for the
     *                  backbone computation
     * @param type      the type of backbone variables that should be computed
     * @return the backbone
     */
    public static Backbone compute(final Collection<Formula> formulas, final Collection<Variable> variables,
                                   final BackboneType type) {
        return compute(formulas, variables, type, null);
    }

    /**
     * Computes the complete backbone for a given collection of formulas w.r.t.
     * a collection of variables and a backbone type.
     * @param formulas  the given collection of formulas
     * @param variables the given collection of relevant variables for the
     *                  backbone computation
     * @return the backbone
     */
    public static Backbone compute(final Collection<Formula> formulas, final Collection<Variable> variables) {
        return compute(formulas, variables, BackboneType.POSITIVE_AND_NEGATIVE);
    }

    /**
     * Computes the backbone for a given collection of formulas w.r.t. a given
     * backbone type.
     * @param formulas the given collection of formulas
     * @param type     the type of backbone variables that should be computed
     * @return the backbone
     */
    public static Backbone compute(final Collection<Formula> formulas, final BackboneType type) {
        return compute(formulas, FormulaHelper.variables(formulas), type);
    }

    /**
     * Computes the complete backbone for a given collection of formulas.
     * @param formulas the given collection of formulas
     * @return the backbone
     */
    public static Backbone compute(final Collection<Formula> formulas) {
        return compute(formulas, FormulaHelper.variables(formulas), BackboneType.POSITIVE_AND_NEGATIVE);
    }

    /**
     * Computes the backbone for a given formula w.r.t. a collection of
     * variables and a backbone type.
     * @param formula   the given formula
     * @param variables the given collection of relevant variables for the
     *                  backbone computation
     * @param type      the type of backbone variables that should be computed
     * @return the backbone
     */
    public static Backbone compute(final Formula formula, final Collection<Variable> variables,
                                   final BackboneType type) {
        return compute(Collections.singletonList(formula), variables, type);
    }

    /**
     * Computes the complete backbone for a given formula w.r.t. a collection of
     * variables and a backbone type.
     * @param formula   the given formula
     * @param variables the given collection of relevant variables for the
     *                  backbone computation
     * @return the backbone
     */
    public static Backbone compute(final Formula formula, final Collection<Variable> variables) {
        return compute(formula, variables, BackboneType.POSITIVE_AND_NEGATIVE);
    }

    /**
     * Computes the backbone for a given formula w.r.t. a given backbone type.
     * @param formula the given formula
     * @param type    the type of backbone variables that should be computed
     * @return the backbone
     */
    public static Backbone compute(final Formula formula, final BackboneType type) {
        return compute(formula, formula.variables(), type);
    }

    /**
     * Computes the complete backbone for a given formula.
     * @param formula the given formula
     * @return the backbone
     */
    public static Backbone compute(final Formula formula) {
        return compute(formula, formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE);
    }

    /**
     * Computes the positive backbone variables for a given collection of
     * formulas w.r.t. a collection of variables.
     * @param formulas  the given collection of formulas
     * @param variables the given collection of relevant variables for the
     *                  backbone computation
     * @return the positive backbone
     */
    public static Backbone computePositive(final Collection<Formula> formulas, final Collection<Variable> variables) {
        return compute(formulas, variables, BackboneType.ONLY_POSITIVE);
    }

    /**
     * Computes the positive backbone variables for a given collection of
     * formulas.
     * @param formulas the given collection of formulas
     * @return the positive backbone
     */
    public static Backbone computePositive(final Collection<Formula> formulas) {
        return compute(formulas, FormulaHelper.variables(formulas), BackboneType.ONLY_POSITIVE);
    }

    /**
     * Computes the positive backbone allVariablesInFormulas for a given formula
     * w.r.t. a collection of variables.
     * @param formula   the given formula
     * @param variables the given collection of relevant variables for the
     *                  backbone computation
     * @return the positive backbone
     */
    public static Backbone computePositive(final Formula formula, final Collection<Variable> variables) {
        return compute(formula, variables, BackboneType.ONLY_POSITIVE);
    }

    /**
     * Computes the positive backbone variables for a given formula.
     * @param formula the given formula
     * @return the positive backbone
     */
    public static Backbone computePositive(final Formula formula) {
        return compute(formula, formula.variables(), BackboneType.ONLY_POSITIVE);
    }

    /**
     * Computes the negative backbone variables for a given collection of
     * formulas w.r.t. a collection of variables.
     * @param formulas  the given collection of formulas
     * @param variables the given collection of relevant variables for the
     *                  backbone computation
     * @return the negative backbone
     */
    public static Backbone computeNegative(final Collection<Formula> formulas, final Collection<Variable> variables) {
        return compute(formulas, variables, BackboneType.ONLY_NEGATIVE);
    }

    /**
     * Computes the negative backbone variables for a given collection of
     * formulas.
     * @param formulas the given collection of formulas
     * @return the negative backbone
     */
    public static Backbone computeNegative(final Collection<Formula> formulas) {
        return compute(formulas, FormulaHelper.variables(formulas), BackboneType.ONLY_NEGATIVE);
    }

    /**
     * Computes the negative backbone variables for a given formula w.r.t. a
     * collection of variables.
     * @param formula   the given formula
     * @param variables the given collection of relevant variables for the
     *                  backbone computation
     * @return the negative backbone
     */
    public static Backbone computeNegative(final Formula formula, final Collection<Variable> variables) {
        return compute(formula, variables, BackboneType.ONLY_NEGATIVE);
    }

    /**
     * Computes the negative backbone variables for a given formula.
     * @param formula the given formula
     * @return the negative backbone
     */
    public static Backbone computeNegative(final Formula formula) {
        return compute(formula, formula.variables(), BackboneType.ONLY_NEGATIVE);
    }
}
