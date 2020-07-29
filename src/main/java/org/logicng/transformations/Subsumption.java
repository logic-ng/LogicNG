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

package org.logicng.transformations;

import org.logicng.datastructures.ubtrees.UBTree;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;

/**
 * A superclass for subsumptions (CNF or DNF).
 * @version 2.0.0
 * @since 1.5.0
 */
public abstract class Subsumption {

    /**
     * Generates a UBTree from the formulas operands (clauses in CNF, minterms in DNF)
     * where all subsumed operands are already deleted.
     * @param formula the formula (must be an n-ary operator and CNF or DNF)
     * @return the UBTree with the operands and deleted subsumed operands
     */
    protected static UBTree<Literal> generateSubsumedUBTree(final Formula formula) {
        final SortedMap<Integer, List<SortedSet<Literal>>> mapping = new TreeMap<>();
        for (final Formula term : formula) {
            mapping.computeIfAbsent(term.literals().size(), k -> new ArrayList<>()).add(term.literals());
        }
        final UBTree<Literal> ubTree = new UBTree<>();
        for (final Map.Entry<Integer, List<SortedSet<Literal>>> entry : mapping.entrySet()) {
            for (final SortedSet<Literal> set : entry.getValue()) {
                if (ubTree.firstSubset(set) == null) {
                    ubTree.addSet(set);
                }
            }
        }
        return ubTree;
    }
}
