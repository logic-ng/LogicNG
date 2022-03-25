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

package org.logicng.knowledgecompilation.bdds.functions;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDOperations;

import java.util.ArrayList;
import java.util.List;

/**
 * Superclass for the normal form generation from a BDD.
 * @version 2.3.0
 * @since 2.3.0
 */
public abstract class BDDNormalFormFunction {

    /**
     * Computes a CNF/DNF from the given BDD.
     * @param bdd the BDD
     * @param cnf {@code true} if a CNF should be computed, {@code false} if a DNF should be computed
     * @return the normal form (CNF or DNF) computed from the BDD
     */
    protected static Formula compute(final BDD bdd, final boolean cnf) {
        final BDDKernel kernel = bdd.underlyingKernel();
        final FormulaFactory f = kernel.factory();
        final List<byte[]> pathsToConstant = cnf
                ? new BDDOperations(kernel).allUnsat(bdd.index())
                : new BDDOperations(kernel).allSat(bdd.index());
        final List<Formula> terms = new ArrayList<>();
        for (final byte[] path : pathsToConstant) {
            final List<Formula> literals = new ArrayList<>();
            for (int i = 0; i < path.length; i++) {
                final Variable var = kernel.getVariableForIndex(i);
                if (path[i] == 0) {
                    literals.add(cnf ? var : var.negate());
                } else if (path[i] == 1) {
                    literals.add(cnf ? var.negate() : var);
                }
            }
            final Formula term = cnf ? f.or(literals) : f.and(literals);
            terms.add(term);
        }
        return cnf ? f.and(terms) : f.or(terms);
    }
}
