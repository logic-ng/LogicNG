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
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDOperations;

import java.util.ArrayList;
import java.util.List;

/**
 * Creates a CNF from a BDD.
 * @version 2.0.0
 * @since 2.0.0
 */
public final class BDDCNFFunction implements BDDFunction<Formula> {

    @Override
    public Formula apply(final BDD bdd) {
        final BDDKernel kernel = bdd.underlyingKernel();
        final List<byte[]> unsatPaths = new BDDOperations(kernel).allUnsat(bdd.index());
        final List<Formula> clauses = new ArrayList<>();
        List<Formula> literals;
        for (final byte[] path : unsatPaths) {
            literals = new ArrayList<>();
            for (int i = 0; i < path.length; i++) {
                if (path[i] == 0) {
                    literals.add(kernel.getVariableForIndex(i));
                } else if (path[i] == 1) {
                    literals.add(kernel.getVariableForIndex(i).negate());
                }
            }
            clauses.add(kernel.factory().or(literals));
        }
        return kernel.factory().and(clauses);
    }
}
