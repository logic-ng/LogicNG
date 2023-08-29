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
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.NAryOperator;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;

/**
 * A superclass for subsumptions (CNF or DNF).
 * @version 2.5.0
 * @since 1.5.0
 */
public abstract class Subsumption {

    protected static Formula compute(final NAryOperator nary, final boolean cnf) {
        final List<SortedSet<Literal>> terms = getTerms(nary);
        final UBTree<Literal> ubTree = UBTree.generateSubsumedUBTree(terms);
        return toNormalForm(ubTree, cnf, nary.factory());
    }

    private static List<SortedSet<Literal>> getTerms(final NAryOperator nary) {
        final List<SortedSet<Literal>> terms = new ArrayList<>();
        for (final Formula term : nary) {
            terms.add(term.literals());
        }
        return terms;
    }

    private static Formula toNormalForm(final UBTree<Literal> ubTree, final boolean cnf, final FormulaFactory f) {
        final List<Formula> terms = new ArrayList<>();
        for (final SortedSet<Literal> term : ubTree.allSets()) {
            terms.add(cnf ? f.or(term) : f.and(term));
        }
        return cnf ? f.cnf(terms) : f.or(terms);
    }
}
