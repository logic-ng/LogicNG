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

package org.logicng.datastructures;

import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

/**
 * An auxiliary variable for encoding results.
 * <p>
 * This variable is used, if the result is added directly to a solver.  In this case no variable on the factory has
 * to be created.
 * @version 1.3
 * @since 1.1
 */
final class EncodingAuxiliaryVariable extends Variable {

    final boolean negated;

    /**
     * Constructs a new auxiliary variable
     * @param name    the literal name
     * @param negated {@code true} if the variables is negated, {@code false} otherwise
     */
    EncodingAuxiliaryVariable(final String name, final boolean negated) {
        super(name, null);
        this.negated = negated;
    }

    @Override
    public Literal negate() {
        return new EncodingAuxiliaryVariable(this.name(), !this.negated);
    }

    @Override
    public String toString() {
        return name();
    }
}
