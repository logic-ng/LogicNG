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
//  Copyright 2015-2018 Christoph Zengler                                //
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

import org.logicng.formulas.And;
import org.logicng.formulas.Equivalence;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Implication;
import org.logicng.formulas.Literal;
import org.logicng.formulas.NAryOperator;
import org.logicng.formulas.Not;
import org.logicng.formulas.Or;
import org.logicng.formulas.PBConstraint;

import java.util.LinkedHashSet;

/**
 * A formula transformation which imports a given formula into a new formula factory.  If the current factory of the
 * formula and the new formula factory are equal, no action is performed.
 * @version 1.3.1
 * @since 1.3.1
 */
public class FormulaFactoryImporter implements FormulaTransformation {

  private final FormulaFactory newFormulaFactory;

  /**
   * Constructs a new formula factory importer with a given formula factory.  This is the formula factory where the
   * formulas should be imported to.
   * @param newFormulaFactory the formula factory where the formulas should be imported to
   */
  public FormulaFactoryImporter(final FormulaFactory newFormulaFactory) {
    this.newFormulaFactory = newFormulaFactory;
  }

  @Override
  public Formula apply(final Formula formula, final boolean cache) {
    if (formula.factory() == this.newFormulaFactory) {
      return formula;
    }
    switch (formula.type()) {
      case TRUE:
        return this.newFormulaFactory.verum();
      case FALSE:
        return this.newFormulaFactory.falsum();
      case LITERAL:
        final Literal literal = (Literal) formula;
        return this.newFormulaFactory.literal(literal.name(), literal.phase());
      case NOT:
        final Not not = (Not) formula;
        return this.newFormulaFactory.not(apply(not.operand(), cache));
      case IMPL:
        final Implication implication = (Implication) formula;
        return this.newFormulaFactory
                .implication(apply(implication.left(), cache), apply(implication.right(), cache));
      case EQUIV:
        final Equivalence equivalence = (Equivalence) formula;
        return this.newFormulaFactory
                .equivalence(apply(equivalence.left(), cache), apply(equivalence.right(), cache));
      case OR:
        final Or or = (Or) formula;
        return this.newFormulaFactory.or(gatherAppliedOperands(or));
      case AND:
        final And and = (And) formula;
        return this.newFormulaFactory.and(gatherAppliedOperands(and));
      case PBC:
        final PBConstraint pbc = (PBConstraint) formula;
        final Literal[] literals = new Literal[pbc.operands().length];
        for (int i = 0; i < pbc.operands().length; i++) {
          literals[i] = (Literal) apply(pbc.operands()[i], cache);
        }
        return this.newFormulaFactory.pbc(pbc.comparator(), pbc.rhs(), literals, pbc.coefficients());
      case NONE:
        return null;
      default:
        throw new IllegalArgumentException("Unknown LogicNG formula type: " + formula.type());
    }
  }

  /**
   * Gather the operands of an n-ary operator and returns its applied operands.
   * @param operator the n-ary operator
   * @return the applied operands of the given operator
   */
  private LinkedHashSet<Formula> gatherAppliedOperands(final NAryOperator operator) {
    final LinkedHashSet<Formula> applied = new LinkedHashSet<>();
    for (final Formula operand : operator) {
      applied.add(apply(operand, false));
    }
    return applied;
  }
}
