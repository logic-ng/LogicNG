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

package org.logicng.pseudobooleans;

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

import java.util.List;

/**
 * The interface for pseudo Boolean constraint encodings.
 * @version 1.1
 * @since 1.1
 */
public interface PBEncoding {

    /**
     * Encodes a pseudo-Boolean constraint and returns its CNF encoding.
     * @param lits   the literals of the constraint
     * @param coeffs the coefficients of the constraint
     * @param rhs    the right hand side of the constraint
     * @param result the current result CNF
     * @return the CNF encoding of the constraint
     */
    List<Formula> encode(final LNGVector<Literal> lits, final LNGIntVector coeffs, int rhs, final List<Formula> result);
}
