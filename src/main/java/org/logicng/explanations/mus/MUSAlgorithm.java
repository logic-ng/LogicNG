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

package org.logicng.explanations.mus;

import org.logicng.explanations.UNSATCore;
import org.logicng.formulas.FormulaFactory;
import org.logicng.propositions.Proposition;

import java.util.List;

/**
 * Abstract super class for MUS computation algorithms.
 * @version 1.3
 * @since 1.1
 */
abstract class MUSAlgorithm {

    /**
     * Computes a MUS for the given propositions.
     * @param propositions the propositions
     * @param f            the formula factory
     * @param config       the MUS configuration
     * @param <T>          the type of the MUSes propositions
     * @return the MUS
     */
    public abstract <T extends Proposition> UNSATCore<T> computeMUS(final List<T> propositions, final FormulaFactory f,
                                                                    final MUSConfig config);
}
