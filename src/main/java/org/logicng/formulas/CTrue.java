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

package org.logicng.formulas;

import org.logicng.datastructures.Assignment;

/**
 * Boolean constant "True".
 * @version 1.0
 * @since 1.0
 */
public final class CTrue extends Constant {

    /**
     * Constructor.
     * @param factory the factory which created this instance
     */
    CTrue(final FormulaFactory factory) {
        super(FType.TRUE, factory);
    }

    @Override
    public boolean evaluate(final Assignment assignment) {
        return true;
    }

    @Override
    public Constant negate() {
        return this.f.falsum();
    }

    @Override
    public int hashCode() {
        return 42;
    }

    @Override
    public boolean equals(final Object other) {
        return other instanceof CTrue;
    }
}
