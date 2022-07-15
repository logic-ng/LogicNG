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

package org.logicng.transformations.dnf;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.transformations.BDDNormalFormTransformation;

/**
 * Transformation of a formula in DNF by converting it to a BDD.
 * @version 2.3.0
 * @since 2.3.0
 */
public final class BDDDNFTransformation extends BDDNormalFormTransformation {

    /**
     * Constructs a new BDD-based DNF transformation with an optional BDD kernel.
     * <p>
     * Warning: You can use this object for arbitrarily many transformations, <b>but</b>
     * the number of different variables in all applied formulas <b>must not exceed</b>
     * the number of variables in the kernel.
     * @param kernel the optional BDD kernel
     */
    public BDDDNFTransformation(final BDDKernel kernel) {
        super(kernel);
    }

    /**
     * Constructs a new BDD-based DNF transformation for a given number of variables.
     * <p>
     * Warning: You can use this object for arbitrarily many transformations, <b>but</b>
     * the number of different variables in all applied formulas <b>must not exceed</b>
     * {@code numVars}.
     * <p>
     * To improve performance you might want to use {@link #BDDDNFTransformation(BDDKernel)},
     * where you have full control over the node and cache size in the used BDD kernel.
     * @param f       the formula factory to use
     * @param numVars the number of variables
     */
    public BDDDNFTransformation(final FormulaFactory f, final int numVars) {
        super(f, numVars);
    }

    /**
     * Constructs a new BDD-based DNF transformation and constructs a new BDD kernel
     * for every formula application.
     */
    public BDDDNFTransformation() {
        this(null);
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        return compute(formula, false, cache);
    }
}
