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

package org.logicng.transformations.qe;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;

import java.util.Arrays;
import java.util.Collection;

/**
 * This transformation eliminates a number of universally quantified variables by replacing them with the Shannon
 * expansion.  If {@code x} is eliminated from a formula {@code f}, the resulting formula is
 * {@code f[true/x] & f[false/x]}.
 * <p>
 * This transformation cannot be cached since it is dependent on the set of literals to eliminate.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class UniversalQuantifierElimination implements FormulaTransformation {

  private final Literal[] elimination;

  /**
   * Constructs a new universal quantifier elimination for a given literal.
   * @param lit the literal
   */
  public UniversalQuantifierElimination(final Literal lit) {
    this.elimination = new Literal[]{lit};
  }

  /**
   * Constructs a new universal quantifier elimination for a given collection of literals.
   * @param literals the collection of literals
   */
  public UniversalQuantifierElimination(final Collection<Literal> literals) {
    this.elimination = literals.toArray(new Literal[literals.size()]);
  }

  /**
   * Constructs a new universal quantifier elimination for the given literals.
   * @param literals the literals
   */
  public UniversalQuantifierElimination(final Literal... literals) {
    this.elimination = Arrays.copyOf(literals, literals.length);
  }

  @Override
  public Formula apply(final Formula formula, boolean cache) {
    Formula result = formula;
    final FormulaFactory f = formula.factory();
    for (final Literal lit : elimination)
      result = f.and(result.restrict(new Assignment(lit)), result.restrict(new Assignment(lit.negate())));
    return result;
  }
}
