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

package org.logicng.propositions;

import org.logicng.collections.ImmutableFormulaList;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;

import java.util.Collection;
import java.util.Objects;

/**
 * A proposition in LogicNG.  A proposition is a collection of formulas with an additional textual description.
 * @version 1.1
 * @since 1.0
 */
public final class StandardProposition extends Proposition {

  private final ImmutableFormulaList formulas;
  private final String description;

  /**
   * Constructs a new proposition for a single formulas.
   * @param formula the formulas
   */
  public StandardProposition(final Formula formula) {
    this.formulas = new ImmutableFormulaList(FType.AND, formula);
    this.description = "";
  }

  /**
   * Constructs a new proposition for a single formulas.
   * @param description the description
   * @param formula     the formulas
   */
  public StandardProposition(final String description, final Formula formula) {
    this.formulas = new ImmutableFormulaList(FType.AND, formula);
    this.description = description == null ? "" : description;
  }

  /**
   * Constructs a new proposition for a collection of formulas.
   * @param description the description
   * @param formulas    the formulas
   */
  public StandardProposition(final String description, final Collection<? extends Formula> formulas) {
    this.formulas = new ImmutableFormulaList(FType.AND, formulas);
    this.description = description == null ? "" : description;
  }

  /**
   * Constructs a new proposition for a variable number of formulas.
   * @param description the description
   * @param formulas    the formulas
   */
  public StandardProposition(final String description, final Formula... formulas) {
    this.formulas = new ImmutableFormulaList(FType.AND, formulas);
    this.description = description == null ? "" : description;
  }

  /**
   * Constructs a new proposition for a immutable formula list.
   * @param description the description
   * @param formulas    the formulas
   */
  public StandardProposition(final String description, final ImmutableFormulaList formulas) {
    this.formulas = new ImmutableFormulaList(FType.AND, formulas);
    this.description = description == null ? "" : description;
  }

  @Override
  public ImmutableFormulaList formulas() {
    return this.formulas;
  }

  /**
   * Returns the bagpack of this proposition.
   * @return the bagpack of this proposition
   */
  public String description() {
    return this.description;
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.formulas, this.description);
  }

  @Override
  public boolean equals(final Object other) {
    if (this == other)
      return true;
    if (other instanceof StandardProposition) {
      final StandardProposition o = (StandardProposition) other;
      return Objects.equals(this.formulas, o.formulas) && Objects.equals(this.description, o.description);
    }
    return false;
  }

  @Override
  public String toString() {
    return String.format("StandardProposition{formulas=%s, description=%s}", this.formulas, this.description);
  }
}
