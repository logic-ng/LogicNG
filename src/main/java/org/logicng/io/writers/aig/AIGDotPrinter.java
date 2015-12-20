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
//  Copyright 2015 Christoph Zengler                                     //
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

package org.logicng.io.writers.aig;

import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Not;
import org.logicng.predicates.AIGPredicate;
import org.logicng.transformations.AIGTransformation;

import java.util.HashMap;
import java.util.Map;

/**
 * A dot file printer for AIGs.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class AIGDotPrinter {

  private Formula formula;
  private Map<Formula, Integer> ids;
  private StringBuilder sb;

  /**
   * Constructor.
   */
  public AIGDotPrinter() {
    // do nothing...
  }

  /**
   * Generates the dot representation for a given formula's AIG.
   * @param formula       the formula
   * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
   * @return the dot representation
   */
  public String createDotString(final Formula formula, boolean alignLiterals) {
    this.formula = formula;
    if (!formula.holds(new AIGPredicate()))
      this.formula = formula.transform(new AIGTransformation());
    this.sb = new StringBuilder("digraph G {\n");
    this.initIDs(alignLiterals);
    this.generateAIGDotString(formula);
    this.generateStartNode();
    this.sb.append("}\n");
    return this.sb.toString();
  }

  /**
   * Initializes the IDs for the nodes.
   * @param alignLiterals indicates whether all literals should be aligned at the same vertical level
   */
  private void initIDs(boolean alignLiterals) {
    if (alignLiterals && !formula.variables().isEmpty())
      this.sb.append("{ rank = same;\n");
    this.ids = new HashMap<>();
    int id = 0;
    for (final Literal lit : this.formula.variables()) {
      this.ids.put(lit, id);
      this.sb.append("  id").append(id).append(" [shape=box, label=\"").append(lit.name()).append("\"];\n");
      id++;
    }
    if (alignLiterals && !formula.literals().isEmpty())
      this.sb.append("}\n");
  }

  /**
   * Adds a dot file string representation of a given formula's AIG to the given dot file string builder.
   * @param formula the formula
   */
  private void generateAIGDotString(final Formula formula) {
    switch (formula.type()) {
      case FALSE:
        sb.append("  false;\n");
        break;
      case TRUE:
        sb.append("  true;\n");
        break;
      case NOT:
        final Formula operand = ((Not) formula).operand();
        if (!ids.containsKey(operand))
          generateAIGDotString(operand);
        break;
      case AND:
        for (final Formula op : formula)
          if (!ids.containsKey(op))
            generateAIGDotString(op);
        final int id = ids.size();
        ids.put(formula, id);
        sb.append("  id").append(id).append(" [label=\"∧\"];\n");
        for (final Formula op : formula) {
          if (op.type() == FType.LITERAL) {
            final Literal lit = (Literal) op;
            if (lit.phase())
              sb.append("  id").append(id).append(" -> id").append(ids.get(op)).append(";\n");
            else
              sb.append("  id").append(id).append(" -> id").append(ids.get(op.negate())).append(" [style=dotted];\n");
          } else if (op.type() == FType.NOT) {
            final Not not = (Not) op;
            sb.append("  id").append(id).append(" -> id").append(ids.get(not.operand())).append(" [style=dotted];\n");
          } else
            throw new UnsupportedOperationException("The AIG-to-dot conversion requires an AIG as input");
        }
      default:
        throw new UnsupportedOperationException("The AIG-to-dot conversion requires an AIG as input");
    }
  }

  /**
   * Generates the start node of the AIG.
   */
  private void generateStartNode() {
    int id = this.ids.size();
    this.sb.append("  id").append(id).append(" [shape=none, label=\"start\"];\n");
    if (this.formula.type() == FType.NOT || this.formula.type() == FType.LITERAL && !((Literal) this.formula).phase())
      this.sb.append("  id").append(id).append(" -> id").append(this.ids.get(this.formula.negate())).append(" [style=dotted];\n");
    else
      this.sb.append("  id").append(id).append(" -> id").append(this.ids.get(this.formula)).append(";\n");
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
