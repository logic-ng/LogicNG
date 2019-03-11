package org.logicng.transformations;

import org.logicng.backbones.Backbone;
import org.logicng.backbones.BackboneGeneration;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;

public class BackboneSimplifier implements FormulaTransformation {
    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final Backbone backbone = BackboneGeneration.compute(formula);
        if (backbone == null) {
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
