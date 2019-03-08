package org.logicng.explanations.backbones;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.Collection;
import java.util.Collections;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Convenient methods for backbone computation.
 * <p>
 * Note: For more control over the backbone solver create an instance of {@link MiniSatBackbone} directly.
 * E.g., with an instance of {@link MiniSatBackbone} the already loaded formulas can be re-used for multiple
 * backbone computations.
 *
 * @version 1.5
 * @since 1.5
 */
public class BackboneGeneration {

    private static final MiniSatBackbone solver = new MiniSatBackbone();

    /**
     * Private constructor.
     */
    private BackboneGeneration() {
        // Intentionally left empty.
    }

    private static SortedSet<Variable> variables(final Collection<Formula> formulas) {
        final SortedSet<Variable> variables = new TreeSet<>();
        for (final Formula formula : formulas) {
            variables.addAll(formula.variables());
        }
        return variables;
    }

    /**
     * Sets a new backbone configuration
     *
     * @param config the new backbone configuration
     */
    public static void setConfig(final BackboneConfig config) {
        solver.setConfig(config);
    }

    /**
     * Computes the backbone for a given collection of formulas w.r.t. a collection of variables and a backbone type.
     *
     * @param formulas  the given collection of formulas
     * @param variables the given collection of relevant variables for the backbone computation
     * @param type      the type of backbone variables that should be computed:
     *                  {@link BackboneType#ONLY_POSITIVE}: Only positive backbone variables are computed
     *                  {@link BackboneType#ONLY_NEGATIVE}: Only negative backbone variables are computed
     *                  {@link BackboneType#POSITIVE_AND_NEGATIVE}: Both, positive and negative backbone variables are computed
     * @return the backbone
     */
    public static Backbone compute(final Collection<Formula> formulas, final Collection<Variable> variables, final BackboneType type) {
        solver.reset();
        solver.add(formulas);
        return solver.compute(variables, type);
    }

    /**
     * Computes the complete backbone for a given collection of formulas w.r.t. a collection of variables and a backbone type.
     *
     * @param formulas  the given collection of formulas
     * @param variables the given collection of relevant variables for the backbone computation
     * @return the backbone
     */
    public static Backbone compute(final Collection<Formula> formulas, final Collection<Variable> variables) {
        return compute(formulas, variables, BackboneType.POSITIVE_AND_NEGATIVE);
    }

    /**
     * Computes the backbone for a given collection of formulas w.r.t. a given backbone type.
     *
     * @param formulas the given collection of formulas
     * @param type     the type of backbone variables that should be computed:
     *                 {@link BackboneType#ONLY_POSITIVE}: Only positive backbone variables are computed
     *                 {@link BackboneType#ONLY_NEGATIVE}: Only negative backbone variables are computed
     *                 {@link BackboneType#POSITIVE_AND_NEGATIVE}: Both, positive and negative backbone variables are computed
     * @return the backbone
     */
    public static Backbone compute(final Collection<Formula> formulas, final BackboneType type) {
        return compute(formulas, variables(formulas), type);
    }

    /**
     * Computes the complete backbone for a given collection of formulas.
     *
     * @param formulas the given collection of formulas
     * @return the backbone
     */
    public static Backbone compute(final Collection<Formula> formulas) {
        return compute(formulas, variables(formulas), BackboneType.POSITIVE_AND_NEGATIVE);
    }

    /**
     * Computes the backbone for a given formula w.r.t. a collection of variables and a backbone type.
     *
     * @param formula   the given formula
     * @param variables the given collection of relevant variables for the backbone computation
     * @param type      the type of backbone variables that should be computed:
     *                  {@link BackboneType#ONLY_POSITIVE}: Only positive backbone variables are computed
     *                  {@link BackboneType#ONLY_NEGATIVE}: Only negative backbone variables are computed
     *                  {@link BackboneType#POSITIVE_AND_NEGATIVE}: Both, positive and negative backbone variables are computed
     * @return the backbone
     */
    public static Backbone compute(final Formula formula, final Collection<Variable> variables, final BackboneType type) {
        return compute(Collections.singletonList(formula), variables, type);
    }

    /**
     * Computes the complete backbone for a given formula w.r.t. a collection of variables and a backbone type.
     *
     * @param formula   the given formula
     * @param variables the given collection of relevant variables for the backbone computation
     * @return the backbone
     */
    public static Backbone compute(final Formula formula, final Collection<Variable> variables) {
        return compute(formula, variables, BackboneType.POSITIVE_AND_NEGATIVE);
    }

    /**
     * Computes the backbone for a given formula w.r.t. a given backbone type.
     *
     * @param formula the given formula
     * @param type    the type of backbone variables that should be computed:
     *                {@link BackboneType#ONLY_POSITIVE}: Only positive backbone variables are computed
     *                {@link BackboneType#ONLY_NEGATIVE}: Only negative backbone variables are computed
     *                {@link BackboneType#POSITIVE_AND_NEGATIVE}: Both, positive and negative backbone variables are computed
     * @return the backbone
     */
    public static Backbone compute(final Formula formula, final BackboneType type) {
        return compute(formula, formula.variables(), type);
    }

    /**
     * Computes the complete backbone for a given formula.
     *
     * @param formula the given formula
     * @return the backbone
     */
    public static Backbone compute(final Formula formula) {
        return compute(formula, formula.variables(), BackboneType.POSITIVE_AND_NEGATIVE);
    }

    /**
     * Computes the positive backbone variables for a given collection of formulas w.r.t. a collection of variables.
     *
     * @param formulas  the given collection of formulas
     * @param variables the given collection of relevant variables for the backbone computation
     * @return the positive backbone
     */
    public static Backbone computePositive(final Collection<Formula> formulas, final Collection<Variable> variables) {
        return compute(formulas, variables, BackboneType.ONLY_POSITIVE);
    }

    /**
     * Computes the positive backbone variables for a given collection of formulas.
     *
     * @param formulas the given collection of formulas
     * @return the positive backbone
     */
    public static Backbone computePositive(final Collection<Formula> formulas) {
        return compute(formulas, variables(formulas), BackboneType.ONLY_POSITIVE);
    }

    /**
     * Computes the positive backbone variables for a given formula w.r.t. a collection of variables.
     *
     * @param formula   the given formula
     * @param variables the given collection of relevant variables for the backbone computation
     * @return the positive backbone
     */
    public static Backbone computePositive(final Formula formula, final Collection<Variable> variables) {
        return compute(formula, variables, BackboneType.ONLY_POSITIVE);
    }

    /**
     * Computes the positive backbone variables for a given formula.
     *
     * @param formula the given formula
     * @return the positive backbone
     */
    public static Backbone computePositive(final Formula formula) {
        return compute(formula, formula.variables(), BackboneType.ONLY_POSITIVE);
    }

    /**
     * Computes the negative backbone variables for a given collection of formulas w.r.t. a collection of variables.
     *
     * @param formulas  the given collection of formulas
     * @param variables the given collection of relevant variables for the backbone computation
     * @return the negative backbone
     */
    public static Backbone computeNegative(final Collection<Formula> formulas, final Collection<Variable> variables) {
        return compute(formulas, variables, BackboneType.ONLY_NEGATIVE);
    }

    /**
     * Computes the negative backbone variables for a given collection of formulas.
     *
     * @param formulas the given collection of formulas
     * @return the negative backbone
     */
    public static Backbone computeNegative(final Collection<Formula> formulas) {
        return compute(formulas, variables(formulas), BackboneType.ONLY_NEGATIVE);
    }

    /**
     * Computes the negative backbone variables for a given formula w.r.t. a collection of variables.
     *
     * @param formula   the given formula
     * @param variables the given collection of relevant variables for the backbone computation
     * @return the negative backbone
     */
    public static Backbone computeNegative(final Formula formula, final Collection<Variable> variables) {
        return compute(formula, variables, BackboneType.ONLY_NEGATIVE);
    }

    /**
     * Computes the negative backbone variables for a given formula.
     *
     * @param formula the given formula
     * @return the negative backbone
     */
    public static Backbone computeNegative(final Formula formula) {
        return compute(formula, formula.variables(), BackboneType.ONLY_NEGATIVE);
    }
}
