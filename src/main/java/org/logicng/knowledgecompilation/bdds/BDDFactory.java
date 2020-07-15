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

/*========================================================================
           Copyright (C) 1996-2002 by Jorn Lind-Nielsen
                        All rights reserved

Permission is hereby granted, without written agreement and without
license or royalty fees, to use, reproduce, prepare derivative
works, distribute, and display this software and its documentation
for any purpose, provided that (1) the above copyright notice and
the following two paragraphs appear in all copies of the source code
and (2) redistributions, including without limitation binaries,
reproduce these notices in the supporting documentation. Substantial
modifications to this software may be copyrighted by their authors
and need not follow the licensing terms described here, provided
that the new terms are clearly indicated in all files where they apply.

IN NO EVENT SHALL JORN LIND-NIELSEN, OR DISTRIBUTORS OF THIS
SOFTWARE BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL,
INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THIS
SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE AUTHORS OR ANY OF THE
ABOVE PARTIES HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

JORN LIND-NIELSEN SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE NO
OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
========================================================================*/

package org.logicng.knowledgecompilation.bdds;

import org.logicng.formulas.And;
import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Implication;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Not;
import org.logicng.handlers.BDDHandler;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDConstruction;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

import java.util.Iterator;

/**
 * The factory for the jBuddy implementation.
 * @version 2.0.0
 * @since 1.4.0
 */
public final class BDDFactory {

    private BDDFactory() {
        // not to be instantiated
    }

    /**
     * Builds a BDD for a given formula.  BDDs support all Boolean formula types but not pseudo-Boolean constraints.
     * The reason is that before converting a formula to a BDD one must specify the number of variables.  In case of
     * pseudo-Boolean constraints this number depends on the translation of the constraint.  Therefore the caller first
     * has to transform any pseudo-Boolean constraints in their respective CNF representation before converting them
     * to a BDD.
     * @param formula the formula
     * @return the top node of the BDD
     */
    public static BDD build(final Formula formula) {
        return build(formula, null, null);
    }

    /**
     * Builds a BDD for a given formula.  BDDs support all Boolean formula types but not pseudo-Boolean constraints.
     * The reason is that before converting a formula to a BDD one must specify the number of variables.  In case of
     * pseudo-Boolean constraints this number depends on the translation of the constraint.  Therefore the caller first
     * has to transform any pseudo-Boolean constraints in their respective CNF representation before converting them
     * to a BDD.
     * @param formula the formula
     * @param kernel  the BBD kernel to use
     * @return the top node of the BDD or {@link BDDKernel#BDD_ABORT} if the computation was aborted
     */
    public static BDD build(final Formula formula, final BDDKernel kernel) {
        return build(formula, kernel, null);
    }

    /**
     * Builds a BDD for a given formula.  BDDs support all Boolean formula types but not pseudo-Boolean constraints.
     * The reason is that before converting a formula to a BDD one must specify the number of variables.  In case of
     * pseudo-Boolean constraints this number depends on the translation of the constraint.  Therefore the caller first
     * has to transform any pseudo-Boolean constraints in their respective CNF representation before converting them
     * to a BDD.
     * <p>
     * If a BDD handler is given and the BDD generation is aborted due to the handler, the method will return
     * {@link BDDKernel#BDD_ABORT} as result. If {@code null} is passed as handler, the generation will continue without
     * interruption.
     * @param formula the formula
     * @param kernel  the BBD kernel to use
     * @param handler the BDD handler
     * @return the top node of the BDD or {@link BDDKernel#BDD_ABORT} if the computation was aborted
     */
    public static BDD build(final Formula formula, final BDDKernel kernel, final BDDHandler handler) {
        if (handler != null) {
            handler.started();
        }
        final int varNum = formula.variables().size();
        final BDDKernel bddKernel = kernel == null
                ? new BDDKernel(formula.factory(), varNum, varNum * 30, varNum * 20)
                : kernel;
        return new BDD(buildRec(formula, bddKernel, new BDDConstruction(bddKernel), handler), bddKernel);
    }

    /**
     * Recursive build procedure for the BDD.
     * <p>
     * If a BDD handler is given and the BDD generation is aborted due to the handler, the method will return
     * {@link BDDKernel#BDD_ABORT} as result. If {@code null} is passed as handler, the generation will continue without
     * interruption.
     * @param formula      the formula
     * @param kernel       the BDD kernel
     * @param construction
     * @param handler      the BDD handler
     * @return the BDD index or {@link BDDKernel#BDD_ABORT} if the computation was aborted
     */
    private static int buildRec(final Formula formula, final BDDKernel kernel, final BDDConstruction construction, final BDDHandler handler) {
        switch (formula.type()) {
            case FALSE:
                return BDDKernel.BDD_FALSE;
            case TRUE:
                return BDDKernel.BDD_TRUE;
            case LITERAL:
                final Literal lit = (Literal) formula;
                final int idx = kernel.getOrAddVarIndex(lit.variable());
                return lit.phase() ? construction.ithVar(idx) : construction.nithVar(idx);
            case NOT: {
                final Not not = (Not) formula;
                final int operand = buildRec(not.operand(), kernel, construction, handler);
                if (operand == BDDKernel.BDD_ABORT) {
                    return BDDKernel.BDD_ABORT;
                }
                final int res = kernel.addRef(construction.not(operand), handler);
                kernel.delRef(operand);
                return res;
            }
            case IMPL:
            case EQUIV:
                final BinaryOperator binary = (BinaryOperator) formula;
                final int left = buildRec(binary.left(), kernel, construction, handler);
                if (left == BDDKernel.BDD_ABORT) {
                    return BDDKernel.BDD_ABORT;
                }
                final int right = buildRec(binary.right(), kernel, construction, handler);
                if (right == BDDKernel.BDD_ABORT) {
                    return BDDKernel.BDD_ABORT;
                }
                int res = kernel.addRef(binary instanceof Implication ? construction.implication(left, right) : construction.equivalence(left, right), handler);
                kernel.delRef(left);
                kernel.delRef(right);
                return res;
            case AND:
            case OR: {
                final Iterator<Formula> it = formula.iterator();
                res = buildRec(it.next(), kernel, construction, handler);
                if (res == BDDKernel.BDD_ABORT) {
                    return BDDKernel.BDD_ABORT;
                }
                while (it.hasNext()) {
                    final int operand = buildRec(it.next(), kernel, construction, handler);
                    if (operand == BDDKernel.BDD_ABORT) {
                        return BDDKernel.BDD_ABORT;
                    }
                    final int previous = res;
                    res = formula instanceof And
                            ? kernel.addRef(construction.and(res, operand), handler)
                            : kernel.addRef(construction.or(res, operand), handler);
                    kernel.delRef(previous);
                    kernel.delRef(operand);
                }
                return res;
            }
            case PBC:
                return buildRec(formula.nnf(), kernel, construction, handler);
            default:
                throw new IllegalArgumentException("Unsupported operator for BDD generation: " + formula.type());
        }
    }
}
