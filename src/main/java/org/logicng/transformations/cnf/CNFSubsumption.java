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

package org.logicng.transformations.cnf;

import org.logicng.datastructures.ubtrees.UBTree;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;
import org.logicng.predicates.CNFPredicate;
import org.logicng.transformations.Subsumption;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;

/**
 * This transformation performs subsumption on a given CNF and returns a new CNF.
 * I.e. performs as many subsumptions as possible.  A subsumption in a CNF means,
 * that e.g. a clause {@code A | B | C} is subsumed by another clause {@code A | B}
 * and can therefore be deleted for an equivalent CNF.
 * @version 2.0.0
 * @since 1.5.0
 */
public final class CNFSubsumption extends Subsumption implements FormulaTransformation {

    @Override
    public Formula apply(final Formula formula, final boolean cache) {
        if (!formula.holds(CNFPredicate.get())) {
            throw new IllegalArgumentException("CNF subsumption can only be applied to formulas in CNF");
        }
        if (formula.type().precedence() >= FType.LITERAL.precedence() || formula.type() == FType.OR) {
            return formula;
        }
        assert formula.type() == FType.AND;
        final UBTree<Literal> ubTree = generateSubsumedUBTree(formula);
        final List<Formula> clauses = new ArrayList<>();
        for (final SortedSet<Literal> literals : ubTree.allSets()) {
            clauses.add(formula.factory().clause(literals));
        }
        return formula.factory().cnf(clauses);
    }
}
