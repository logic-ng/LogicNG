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

package org.logicng.formulas.cache;

/**
 * The pre-defined predicate cache entries.
 * @version 1.5.1
 * @since 1.0
 */
public enum PredicateCacheEntry implements CacheEntry {
    IS_NNF("negation normal form"),
    IS_CNF("conjunctive normal form"),
    IS_DNF("disjunctive normal form"),
    IS_AIG("and-inverter graph"),
    IS_SAT("satisfiable"),
    IS_TAUTOLOGY("tautology");

    private final String description;

    /**
     * Constructs a new entry.
     * @param description the description of this entry
     */
    PredicateCacheEntry(final String description) {
        this.description = description;
    }

    @Override
    public String description() {
        return "PredicateCacheEntry{description=" + description + "}";
    }
}
