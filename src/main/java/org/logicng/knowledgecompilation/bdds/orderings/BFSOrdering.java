// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

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
 * A breadth-first-search BDD variable ordering. Traverses the formula in a BFS
 * manner and gathers all variables in the occurrence.
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
