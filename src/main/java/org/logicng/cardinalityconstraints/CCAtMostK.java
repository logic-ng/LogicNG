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

package org.logicng.cardinalityconstraints;

import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Variable;

/**
 * The interface for at-most-k (AMK) cardinality constraints.
 * @version 2.0.0
 * @since 1.0
 */
public interface CCAtMostK {

    /**
     * Builds a cardinality constraint of the form {@code var_1 + var_2 + ... + var_n <= k}.
     * @param result the result for the encoding
     * @param vars   the variables {@code var_1 ... var_n}
     * @param rhs    the right hand side {@code k} of the constraint
     * @throws IllegalArgumentException if the right hand side of the cardinality constraint is negative
     */
    void build(final EncodingResult result, final Variable[] vars, int rhs);

    /**
     * Returns the incremental data for the current encoded constraint.
     * @return the incremental data for the current encoded constraint
     */
    CCIncrementalData incrementalData();
}
