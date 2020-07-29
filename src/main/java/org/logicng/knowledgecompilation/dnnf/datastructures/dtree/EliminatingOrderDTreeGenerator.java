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

package org.logicng.knowledgecompilation.dnnf.datastructures.dtree;

import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;
import org.logicng.predicates.CNFPredicate;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * A generator for a DTree from an arbitrary eliminating order of variables as described in
 * A. Darwiche "Decomposable Negation Normal Form" (algorithm "el2dt").
 * @version 2.0.0
 * @since 2.0.0
 */
public abstract class EliminatingOrderDTreeGenerator implements DTreeGenerator {

    /**
     * Generates the DTree
     * @param cnf      the CNF input formula
     * @param ordering the variable ordering
     * @return the DTree
     */
    public final DTree generateWithEliminatingOrder(final Formula cnf, final List<Variable> ordering) {
        assert cnf.variables().size() == ordering.size();

        if (!cnf.holds(CNFPredicate.get()) || cnf.isAtomicFormula()) {
            throw new IllegalArgumentException("Cannot generate DTree from a non-cnf formula or atomic formula");
        } else if (cnf.type() != FType.AND) {
            return new DTreeLeaf(0, cnf);
        }

        final List<DTree> sigma = new ArrayList<>();
        int id = 0;
        for (final Formula clause : cnf) {
            sigma.add(new DTreeLeaf(id++, clause));
        }

        for (final Variable variable : ordering) {
            final List<DTree> gamma = new ArrayList<>();
            final Iterator<DTree> sigmaIterator = sigma.iterator();
            while (sigmaIterator.hasNext()) {
                final DTree tree = sigmaIterator.next();
                if (tree.staticVariableSet().contains(variable)) {
                    gamma.add(tree);
                    sigmaIterator.remove();
                }
            }
            if (!gamma.isEmpty()) {
                sigma.add(compose(gamma));
            }
        }

        return compose(sigma);
    }

    protected DTree compose(final List<DTree> trees) {
        assert !trees.isEmpty();

        if (trees.size() == 1) {
            return trees.get(0);
        } else {
            final DTree left = compose(trees.subList(0, trees.size() / 2));
            final DTree right = compose(trees.subList(trees.size() / 2, trees.size()));
            return new DTreeNode(left, right);
        }
    }
}
