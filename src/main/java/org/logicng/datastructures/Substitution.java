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

package org.logicng.datastructures;

import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.HashMap;
import java.util.Map;

/**
 * A Boolean substitution.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public class Substitution {

  private final Map<Literal, Formula> subst;

  /**
   * Constructs a new empty substitution.
   */
  public Substitution() {
    this.subst = new HashMap<>();
  }

  /**
   * Returns the number of mappings in this substitution.
   * @return the number of mappings in this substitution
   */
  public int size() {
    return subst.size();
  }

  /**
   * Adds a mapping from literal to formula to this substitution.
   * <p>
   * If there is already a mapping for this literal, it will be overwritten.
   * @param literal the literal
   * @param formula the formula
   * @throws IllegalArgumentException if a literal with a negative phase is added
   */
  public void addMapping(final Literal literal, final Formula formula) {
    if (!literal.phase())
      throw new IllegalArgumentException("A substitution can only contain mappings from positive literals: " + literal);
    subst.put(literal, formula);
  }

  /**
   * Returns a formula for a given literal.  If there is no mapping for this literal, {@code null} is returned.
   * @param literal the literal
   * @param f       the formula factory
   * @return an optional formula
   */
  public Formula getSubstitution(final Literal literal, final FormulaFactory f) {
    Formula res = literal.phase() ? subst.get(literal) : subst.get(literal.negate());
    if (res == null)
      return null;
    return literal.phase() ? res : f.not(res);
  }

  @Override
  public int hashCode() {
    return subst.hashCode();
  }

  @Override
  public boolean equals(final Object other) {
    return this == other || (other instanceof Substitution && this.subst.equals(((Substitution) other).subst));
  }

  @Override
  public String toString() {
    return "Substitution" + subst;
  }
}
