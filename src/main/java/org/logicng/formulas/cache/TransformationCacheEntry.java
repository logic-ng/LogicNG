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

package org.logicng.formulas.cache;

/**
 * The pre-defined transformation cache entries.
 * @version 1.3
 * @since 1.0
 */
public enum TransformationCacheEntry implements CacheEntry {
  NNF("negation normal form"),
  PLAISTED_GREENBAUM_POS("Plaisted & Greenbaum conjunctive normal form (positive polarity)"),
  PLAISTED_GREENBAUM_NEG("Plaisted & Greenbaum conjunctive normal form (negative polarity)"),
  PLAISTED_GREENBAUM_VARIABLE("Plaisted & Greenbaum variable"),
  TSEITIN("Tseitin conjunctive normal form"),
  TSEITIN_VARIABLE("Tseitin variable"),
  FACTORIZED_CNF("factorized conjunctive normal form"),
  BDD_CNF("conjunctive normal form via BDD"),
  FACTORIZED_DNF("factorized disjunctive normal form"),
  AIG("and-inverter graph"),
  UNIT_PROPAGATION("unit propagation"),
  DISTRIBUTIVE_SIMPLIFICATION("distributive simplification"),
  ANONYMIZATION("anonymization");

  private final String description;

  /**
   * Constructs a new entry.
   * @param description the description of this entry
   */
  TransformationCacheEntry(final String description) {
    this.description = description;
  }

  @Override
  public String description() {
    return "TransformationCacheEntry{description=" + this.description + "}";
  }
}
