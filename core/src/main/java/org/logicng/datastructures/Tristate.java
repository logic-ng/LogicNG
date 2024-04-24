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

/**
 * A tristate constant.  This constant can have three different values: {@code TRUE}, {@code FALSE}, and {@code UNDEF}.
 * @version 1.0
 * @since 1.0
 */
public enum Tristate {
    TRUE, FALSE, UNDEF;

    /**
     * Returns the negation of a tristate constant.
     * @param l the tristate constant
     * @return the negation of the tristate constant
     */
    public static Tristate negate(final Tristate l) {
        return l == FALSE ? TRUE : (l == TRUE ? FALSE : UNDEF);
    }

    /**
     * Constructs a tristate constant from an ordinary Boolean value.
     * @param b the Boolean value
     * @return the tristate constant
     */
    public static Tristate fromBool(boolean b) {
        return b ? TRUE : FALSE;
    }
}
