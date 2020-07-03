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

package org.logicng.explanations;

import org.logicng.propositions.Proposition;

import java.util.List;
import java.util.Objects;

/**
 * An unsatisfiable core (can be a minimal unsatisfiable sub-formula).
 * @param <T> the type of the core's propositions
 * @version 1.3
 * @since 1.1
 */
public final class UNSATCore<T extends Proposition> {

    private final List<T> propositions;
    private final boolean isMUS;

    /**
     * Constructs a new unsatisfiable core.
     * @param propositions the propositions of the core
     * @param isMUS        {@code true} if it is a MUS, {@code false} otherwise
     */
    public UNSATCore(final List<T> propositions, final boolean isMUS) {
        this.propositions = propositions;
        this.isMUS = isMUS;
    }

    /**
     * Returns the propositions of this MUS.
     * @return the propositions of this MUS
     */
    public List<T> propositions() {
        return this.propositions;
    }

    /**
     * Returns {@code true} if this core is a MUS, {@code false} otherwise.
     * @return {@code true} if this core is a MUS, {@code false} otherwise
     */
    public boolean isMUS() {
        return this.isMUS;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.propositions, this.isMUS);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof UNSATCore)) {
            return false;
        }
        final UNSATCore<?> unsatCore = (UNSATCore<?>) o;
        return this.isMUS == unsatCore.isMUS && Objects.equals(this.propositions, unsatCore.propositions);
    }

    @Override
    public String toString() {
        return String.format("UNSATCore{isMUS=%s, propositions=%s}", this.isMUS, this.propositions);
    }
}
