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

package org.logicng.knowledgecompilation.bdds.orderings;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

/**
 * A breadth-first-search BDD variable ordering.  Traverses the formula in a BFS manner
 * and gathers all variables in the occurrence.
 * @version 2.0.0
 * @since 1.4.0
 */
public final class BFSOrdering implements VariableOrderingProvider {

    @Override
    public List<Variable> getOrder(final Formula formula) {
        return new ArrayList<>(bfs(formula));
    }

    private LinkedHashSet<Variable> bfs(final Formula formula) {
        final LinkedHashSet<Variable> variables = new LinkedHashSet<>();
        final Queue<Formula> queue = new LinkedList<>();
        queue.add(formula);
        while (!queue.isEmpty()) {
            final Formula current = queue.remove();
            switch (current.type()) {
                case LITERAL:
                    final Literal lit = (Literal) current;
                    if (lit.phase()) {
                        variables.add(lit.variable());
                    } else {
                        queue.add(lit.variable());
                    }
                    break;
                case NOT:
                    queue.add(((Not) current).operand());
                    break;
                case IMPL:
                case EQUIV:
                    final BinaryOperator op = (BinaryOperator) current;
                    queue.add(op.left());
                    queue.add(op.right());
                    break;
                case AND:
                case OR:
                    for (final Formula operand : current) {
                        queue.add(operand);
                    }
                    break;
                case PBC:
                    final PBConstraint pbc = (PBConstraint) current;
                    for (final Literal literal : pbc.operands()) {
                        variables.add(literal.variable());
                    }
                    break;
            }
        }
        return variables;
    }
}
