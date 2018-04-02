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

package org.logicng.datastructures;

import org.logicng.formulas.Formula;
import org.logicng.formulas.Variable;

import java.util.HashMap;
import java.util.Map;

/**
 * A Boolean substitution.
 * @version 1.0
 * @since 1.0
 */
public class Substitution {

  protected final Map<Variable, Formula> subst;

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
    return this.subst.size();
  }

  /**
   * Adds a mapping from variable to formula to this substitution.
   * <p>
   * If there is already a mapping for this variable, it will be overwritten.
   * @param variable the variable
   * @param formula  the formula
   */
  public void addMapping(final Variable variable, final Formula formula) {
    this.subst.put(variable, formula);
  }

  /**
   * Returns a formula for a given variable.  If there is no mapping for this variable, {@code null} is returned.
   * @param variable the variable
   * @return an formula of {@code null}
   */
  public Formula getSubstitution(final Variable variable) {
    return this.subst.get(variable);
  }

  @Override
  public int hashCode() {
    return this.subst.hashCode();
  }

  @Override
  public boolean equals(final Object other) {
    return other != null && (this == other || (this.getClass() == other.getClass() && this.subst.equals(((Substitution) other).subst)));
  }

  @Override
  public String toString() {
    return "Substitution" + this.subst;
  }
}
