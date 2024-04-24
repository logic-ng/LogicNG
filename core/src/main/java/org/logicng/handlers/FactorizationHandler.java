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

package org.logicng.handlers;

import org.logicng.formulas.Formula;

/**
 * A handler for factorization methods (CNF, DNF).
 * @version 2.1.0
 * @since 1.0
 */
public interface FactorizationHandler extends Handler {

    /**
     * This method is called every time a distribution is performed.
     * @return {@code true} if the factorization should be continued, otherwise {@code false}
     */
    default boolean performedDistribution() {
        return true;
    }

    /**
     * This method is called every time a new clause is created.
     * @param clause the clause
     * @return {@code true} if the factorization should be continued, otherwise {@code false}
     */
    default boolean createdClause(final Formula clause) {
        return true;
    }
}
