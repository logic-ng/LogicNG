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

package org.logicng.transformations.dnf;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.transformations.CanonicalEnumeration;

/**
 * Canonical DNF generation via enumeration of models by a SAT solver.
 * @version 2.3.0
 * @since 1.0
 */
public final class CanonicalDNFEnumeration extends CanonicalEnumeration implements FormulaTransformation {

    private final static CanonicalDNFEnumeration INSTANCE = new CanonicalDNFEnumeration();

    /**
     * @deprecated In the next version, the standard constructor will be replaced by a private constructor.
     * In order to instantiate an object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public CanonicalDNFEnumeration() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static CanonicalDNFEnumeration get() {
        return INSTANCE;
    }

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        return compute(formula, false);
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
