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

package org.logicng.explanations.unsatcores;

import org.logicng.explanations.unsatcores.algorithms.DeletionBasedMUS;
import org.logicng.explanations.unsatcores.algorithms.PlainInsertionBasedMUS;
import org.logicng.formulas.FormulaFactory;
import org.logicng.propositions.Proposition;

import java.util.List;

/**
 * Computes a minimal unsatisfiable subset (MUS) of a given formula with different algorithms.
 * @version 1.3
 * @since 1.1
 */
public final class MUSGeneration {

  private final DeletionBasedMUS deletion;
  private final PlainInsertionBasedMUS insertion;

  /**
   * Constructs a new MUS generator.
   */
  public MUSGeneration() {
    this.deletion = new DeletionBasedMUS();
    this.insertion = new PlainInsertionBasedMUS();
  }

  /**
   * Computes a MUS for the given propositions with the default algorithm and the default configuration.
   * @param propositions the propositions
   * @param f            the formula factory
   * @param <T>          the type of the MUSes propositions
   * @return the MUS
   */
  public <T extends Proposition> UNSATCore<T> computeMUS(final List<T> propositions, final FormulaFactory f) {
    return this.computeMUS(propositions, f, new MUSConfig.Builder().build());
  }

  /**
   * Computes a MUS for the given propositions and the given configuration of the MUS generation.
   * @param propositions the propositions
   * @param f            the formula factory
   * @param config       the MUS configuration
   * @param <T>          the type of the MUSes propositions
   * @return the MUS
   */
  public <T extends Proposition> UNSATCore<T> computeMUS(final List<T> propositions, final FormulaFactory f, final MUSConfig config) {
    if (propositions.isEmpty())
      throw new IllegalArgumentException("Cannot generate a MUS for an empty list of propositions");
    switch (config.algorithm) {
      case PLAIN_INSERTION:
        return insertion.computeMUS(propositions, f, config);
      case DELETION:
      default:
        return deletion.computeMUS(propositions, f, config);
    }
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }

}
