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
//  Copyright 2015-2016 Christoph Zengler                                //
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

package org.logicng.bdds.orderings;

import org.logicng.formulas.BinaryOperator;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Not;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * A depth-first-search BDD variable ordering.  Traverses the formula in a DFS manner
 * and gathers all variables in the occurrence.
 * @version 1.4.0
 * @since 1.4.0
 */
public class DFSOrdering implements VariableOrderingProvider {

  @Override
  public List<Variable> getOrder(final Formula formula) {
    final LinkedHashSet<Variable> order = new LinkedHashSet<>(formula.variables().size());
    dfs(formula, order);
    return new ArrayList<>(order);
  }

  private void dfs(final Formula formula, final LinkedHashSet<Variable> variables) {
    switch (formula.type()) {
      case LITERAL:
        variables.add(((Literal) formula).variable());
        break;
      case NOT:
        dfs(((Not) formula).operand(), variables);
        break;
      case IMPL:
      case EQUIV:
        final BinaryOperator op = (BinaryOperator) formula;
        dfs(op.left(), variables);
        dfs(op.right(), variables);
        break;
      case AND:
      case OR:
        for (final Formula operand : formula)
          dfs(operand, variables);
        break;
      case PBC:
        final PBConstraint pbc = (PBConstraint) formula;
        for (final Literal lit : pbc.operands())
          variables.add(lit.variable());
        break;
    }
  }
}
