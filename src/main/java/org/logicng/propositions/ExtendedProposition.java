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
 * An extended proposition in LogicNG.  An extended proposition is a collection of formulas with additional information
 * like a user-provided {@link PropositionBagpack} object.
 * @param <T> the type of the bagback
 * @version 1.0
 * @since 1.0
 */
public final class ExtendedProposition<T extends PropositionBagpack> extends Proposition {

  private final ImmutableFormulaList formulas;
  private final T bagback;

  /**
   * Constructs a new extended proposition for a single formula.
   * @param bagback the bagpack
   * @param formula the formula
   */
  public ExtendedProposition(final T bagback, final Formula formula) {
    this.formulas = new ImmutableFormulaList(FType.AND, formula);
    this.bagback = bagback;
  }

  /**
   * Constructs a new extended proposition for a collection of formulas.
   * @param bagback  the bagpack
   * @param formulas the formulas
   */
  public ExtendedProposition(final T bagback, final Collection<? extends Formula> formulas) {
    this.formulas = new ImmutableFormulaList(FType.AND, formulas);
    this.bagback = bagback;
  }

  /**
   * Constructs a new extended proposition for a variable number of formulas.
   * @param bagback  the bagpack
   * @param formulas the formulas
   */
  public ExtendedProposition(final T bagback, final Formula... formulas) {
    this.formulas = new ImmutableFormulaList(FType.AND, formulas);
    this.bagback = bagback;
  }

  /**
   * Constructs a new extended proposition for a immutable list of formulas.
   * @param bagback  the bagpack
   * @param formulas the formulas
   */
  public ExtendedProposition(final T bagback, final ImmutableFormulaList formulas) {
    this.formulas = new ImmutableFormulaList(FType.AND, formulas);
    this.bagback = bagback;
  }

  @Override
  public ImmutableFormulaList formulas() {
    return this.formulas;
  }

  /**
   * Returns the bagpack of this proposition.
   * @return the bagpack of this proposition
   */
  public T bagback() {
    return this.bagback;
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.formulas, this.bagback);
  }

  @Override
  public boolean equals(final Object other) {
    if (this == other)
      return true;
    if (other instanceof ExtendedProposition) {
      final ExtendedProposition o = (ExtendedProposition) other;
      return Objects.equals(this.formulas, o.formulas) && Objects.equals(this.bagback, o.bagback);
    }
    return false;
  }

  @Override
  public String toString() {
    return String.format("ExtendedProposition{formulas=%s, bagpack=%s}", this.formulas, this.bagback);
  }
}
