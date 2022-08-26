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

import static org.logicng.formulas.FType.LITERAL;
import static org.logicng.formulas.cache.TransformationCacheEntry.BDD_CNF;
import static org.logicng.formulas.cache.TransformationCacheEntry.BDD_DNF;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.BDDFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

/**
 * Transformation of a formula in a normal form (DNF or CNF) by converting it to a BDD.
 * @version 2.3.0
 * @since 2.3.0
 */
public abstract class BDDNormalFormTransformation implements FormulaTransformation {

    private final UnitPropagation up = UnitPropagation.get();
    private final BDDKernel kernel;

    /**
     * Constructs a new BDD-based normal form transformation with an optional BDD kernel.
     * <p>
     * Warning: You can use this object for arbitrarily many transformations, <b>but</b>
     * the number of different variables in all applied formulas <b>must not exceed</b>
     * the number of variables in the kernel.
     * @param kernel the optional BDD kernel
     */
    public BDDNormalFormTransformation(final BDDKernel kernel) {
        this.kernel = kernel;
    }

    /**
     * Constructs a new BDD-based normal form transformation for a given number of variables.
     * <p>
     * Warning: You can use this object for arbitrarily many transformations, <b>but</b>
     * the number of different variables in all applied formulas <b>must not exceed</b>
     * {@code numVars}.
     * <p>
     * To improve performance you might want to use {@link #BDDNormalFormTransformation(BDDKernel)},
     * where you have full control over the node and cache size in the used BDD kernel.
     * @param f       the formula factory to use
     * @param numVars the number of variables
     */
    public BDDNormalFormTransformation(final FormulaFactory f, final int numVars) {
        this.kernel = new BDDKernel(f, numVars, Math.max(numVars * 30, 20_000), Math.max(numVars * 20, 20_000));
    }

    /**
     * Computes the CNF or DNF from the given formula by using a BDD.
     * @param formula the formula to transform
     * @param cnf     {@code true} if a CNF should be computed, {@code false} if a canonical DNF should be computed
     * @param cache   indicates whether the result should be cached in this formula's cache
     * @return the normal form (CNF or DNF) of the formula
     */
    protected Formula compute(final Formula formula, final boolean cnf, final boolean cache) {
        if (formula.type().precedence() >= LITERAL.precedence()) {
            return formula;
        }
        if (hasNormalForm(formula, cnf)) {
            return formula;
        }
        final Formula cached = formula.transformationCacheEntry(cnf ? BDD_CNF : BDD_DNF);
        if (cache && cached != null) {
            return cached;
        }
        final BDD bdd = BDDFactory.build(formula, this.kernel, null);
        final Formula normalForm = cnf ? bdd.cnf() : bdd.dnf();
        final Formula simplifiedNormalForm;
        if (cnf) {
            simplifiedNormalForm = normalForm.transform(this.up);
        } else {
            // unit propagation simplification creates a CNF, so we use the negated DNF to negate the result back to DNF again
            simplifiedNormalForm = normalForm.negate().nnf().transform(this.up).negate().nnf();
        }
        if (cache) {
            formula.setTransformationCacheEntry(BDD_CNF, simplifiedNormalForm);
        }
        return simplifiedNormalForm;
    }

    private boolean hasNormalForm(final Formula formula, final boolean cnf) {
        return cnf ? formula.isCNF() : formula.isDNF();
    }
}
