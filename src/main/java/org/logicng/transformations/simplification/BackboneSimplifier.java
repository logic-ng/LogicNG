// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.simplification;

import org.logicng.backbones.Backbone;
import org.logicng.backbones.BackboneType;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.functions.BackboneFunction;

/**
 * This class simplifies a formula by computing its backbone and propagating
 * it through the formula. E.g. in the formula {@code A & B & (A | B | C) & (~B | D)}
 * the backbone {@code A, B} is computed and propagated, yielding the simplified formula
 * {@code A & B & D}.
 * @version 2.3.0
 * @since 1.5.0
 */
public final class BackboneSimplifier implements FormulaTransformation {

    private static final BackboneSimplifier INSTANCE = new BackboneSimplifier();

    /**
     * @deprecated In the next version, the standard constructor will be replaced by a private constructor.
     * In order to instantiate an object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public BackboneSimplifier() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static BackboneSimplifier get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final SATSolver solver = MiniSat.miniSat(formula.factory());
        solver.add(formula);
        final Backbone backbone = solver.execute(BackboneFunction.builder().variables(formula.variables()).type(BackboneType.POSITIVE_AND_NEGATIVE).build());
        if (!backbone.isSat()) {
            return formula.factory().falsum();
        }
        if (!backbone.getNegativeBackbone().isEmpty() || !backbone.getPositiveBackbone().isEmpty()) {
            final Formula backboneFormula = backbone.toFormula(formula.factory());
            final Assignment assignment = new Assignment(backbone.getCompleteBackbone());
            final Formula restrictedFormula = formula.restrict(assignment);
            return formula.factory().and(backboneFormula, restrictedFormula);
        } else {
            return formula;
        }
    }
}
