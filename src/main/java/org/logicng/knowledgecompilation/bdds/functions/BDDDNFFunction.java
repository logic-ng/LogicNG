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

/**
 * Creates a DNF from a BDD.
 * @version 2.3.0
 * @since 2.0.0
 */
public final class BDDDNFFunction extends BDDNormalFormFunction implements BDDFunction<Formula> {

    private final static BDDDNFFunction INSTANCE = new BDDDNFFunction();

    /**
     * Private empty constructor.  Singleton class.
     */
    private BDDDNFFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton of the transformation.
     * @return the transformation instance
     */
    public static BDDDNFFunction get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final BDD bdd) {
        return compute(bdd, false);
    }
}
