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

package org.logicng.propositions;

import org.logicng.formulas.Formula;

import java.util.Objects;

/**
 * An extended proposition in LogicNG.  An extended proposition is a formula with additional information
 * like a user-provided {@link PropositionBackpack} object.
 * @param <T> the type of the bagback
 * @version 2.0.0
 * @since 1.0
 */
public final class ExtendedProposition<T extends PropositionBackpack> extends Proposition {

    private final Formula formula;
    private final T backpack;

    /**
     * Constructs a new extended proposition for a single formula.
     * @param backpack the backpack
     * @param formula  the formula
     */
    public ExtendedProposition(final T backpack, final Formula formula) {
        this.formula = formula;
        this.backpack = backpack;
    }

    @Override
    public Formula formula() {
        return this.formula;
    }

    /**
     * Returns the backpack of this proposition.
     * @return the backpack of this proposition
     */
    public T backpack() {
        return this.backpack;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.formula, this.backpack);
    }

    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }
        if (other instanceof ExtendedProposition) {
            final ExtendedProposition<?> o = (ExtendedProposition<?>) other;
            return Objects.equals(this.formula, o.formula) && Objects.equals(this.backpack, o.backpack);
        }
        return false;
    }

    @Override
    public String toString() {
        return String.format("ExtendedProposition{formula=%s, backpack=%s}", this.formula, this.backpack);
    }
}
