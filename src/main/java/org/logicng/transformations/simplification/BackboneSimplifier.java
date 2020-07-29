///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

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
 * @version 1.5.0
 * @since 1.5.0
 */
public final class BackboneSimplifier implements FormulaTransformation {

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
