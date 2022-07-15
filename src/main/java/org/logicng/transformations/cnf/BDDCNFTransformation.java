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

package org.logicng.transformations.cnf;

import static org.logicng.formulas.FType.LITERAL;
import static org.logicng.formulas.cache.TransformationCacheEntry.BDD_CNF;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.knowledgecompilation.bdds.BDDFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.predicates.CNFPredicate;
import org.logicng.transformations.UnitPropagation;

/**
 * Transformation of a formula in CNF by converting it to a BDD.
 * @version 1.4.0
 * @since 1.4.0
 */
public final class BDDCNFTransformation implements FormulaTransformation {

    private final UnitPropagation up = new UnitPropagation();
    private final BDDKernel kernel;

    /**
     * Constructs a new BDD-based CNF transformation with an optional BDD kernel.
     * <p>
     * Warning: You can use this object for arbitrarily many transformations, <b>but</b>
     * the number of different variables in all applied formulas <b>must not exceed</b>
     * the number of variables in the kernel.
     * @param kernel the optional BDD kernel
     */
    public BDDCNFTransformation(final BDDKernel kernel) {
        this.kernel = kernel;
    }

    /**
     * Constructs a new BDD-based CNF transformation for a given number of variables.
     * <p>
     * Warning: You can use this object for arbitrarily many transformations, <b>but</b>
     * the number of different variables in all applied formulas <b>must not exceed</b>
     * {@code numVars}.
     * <p>
     * To improve performance you might want to use {@link #BDDCNFTransformation(BDDKernel)},
     * where you have full control over the node and cache size in the used BDD kernel.
     * @param f       the formula factory to use
     * @param numVars the number of variables
     */
    public BDDCNFTransformation(final FormulaFactory f, final int numVars) {
        this.kernel = new BDDKernel(f, numVars, Math.max(numVars * 30, 20_000), Math.max(numVars * 20, 20_000));
    }

    /**
     * Constructs a new BDD-based CNF transformation and constructs a new BDD kernel
     * for every formula application.
     */
    public BDDCNFTransformation() {
        this(null);
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        if (formula.type().precedence() >= LITERAL.precedence()) {
            return formula;
        }
        if (formula.holds(CNFPredicate.get())) {
            return formula;
        }
        final Formula cached = formula.transformationCacheEntry(BDD_CNF);
        if (cache && cached != null) {
            return cached;
        }
        final Formula cnf = BDDFactory.build(formula, this.kernel, null).cnf().transform(this.up);
        if (cache) {
            formula.setTransformationCacheEntry(BDD_CNF, cnf);
        }
        return cnf;
    }
}
