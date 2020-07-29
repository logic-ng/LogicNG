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

package org.logicng.transformations;

import static org.logicng.util.FormulaHelper.literalsAsVariables;

import org.logicng.cardinalityconstraints.CCAMOPure;
import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.List;

/**
 * Transformation of a formula to a formula with expanded at-most-one and exactly-one cardinality constraints.
 * Each subformula of the formula that is a pseudo-Boolean constraint of type AMO or EXO gets replaced by a pure encoding such that
 * the resulting formula is equivalent and free of pseudo-Boolean constraints.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class PureExpansionTransformation implements FormulaTransformation {

    private final CCAMOPure amoEncoder = new CCAMOPure();

    /**
     * Constructs a new transformation instance.
     */
    public PureExpansionTransformation() {
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        final FormulaFactory f = formula.factory();
        switch (formula.type()) {
            case FALSE:
            case TRUE:
            case LITERAL:
                return formula;
            case NOT:
                final Not not = (Not) formula;
                return f.not(apply(not.operand(), cache));
            case OR:
            case AND:
                final NAryOperator nary = (NAryOperator) formula;
                final List<Formula> newOps = new ArrayList<>(nary.numberOfOperands());
                for (final Formula op : nary) {
                    newOps.add(apply(op, cache));
                }
                return f.naryOperator(formula.type(), newOps);
            case IMPL:
            case EQUIV:
                final BinaryOperator binary = (BinaryOperator) formula;
                final Formula newLeft = apply(binary.left(), cache);
                final Formula newRight = apply(binary.right(), cache);
                return f.binaryOperator(formula.type(), newLeft, newRight);
            case PBC:
                final PBConstraint pbc = (PBConstraint) formula;
                if (pbc.isAmo() || pbc.isExo()) {
                    final EncodingResult encodingResult = EncodingResult.resultForFormula(f);
                    final Variable[] vars = literalsAsVariables(pbc.operands());
                    this.amoEncoder.build(encodingResult, vars);
                    final List<Formula> encoding = encodingResult.result();
                    if (pbc.isExo()) {
                        encoding.add(f.or(vars));
                    }
                    return f.and(encoding);
                } else {
                    throw new UnsupportedOperationException("Pure encoding for a PBC of type other than AMO or EXO is currently not supported.");
                }
            default:
                throw new IllegalStateException("Unknown formula type: " + formula.type());
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
