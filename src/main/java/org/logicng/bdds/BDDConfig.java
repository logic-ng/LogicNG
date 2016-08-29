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

package org.logicng.bdds;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;

/**
 * The BDD object for BDD configurations.
 * @version 1.2
 * @since 1.2
 */
public final class BDDConfig extends Configuration {

  /**
   * The different factories for BDDs.
   * {@code CLASSICAL} - a classical BDD factory
   * {@code COMPLEMENTARY} - a BDD factory with complementary edges
   * {@code BUDDY} - the Buddy BDD factory
   */
  public enum FactoryType {
    CLASSICAL, COMPLEMENTARY, BUDDY
  }

  /**
   * The different variable orders for a BDD.
   * {@code FCFS} - first come first serve - the order in which the variables appear in the formula
   * {@code INORDER} - ordered by number of appearances (from large to small)
   * {@code REVERSE} - ordered by number of appearances (from small to large)
   * {@code DWAM} - dynamic weight assignment method
   * {@code MANUAL} - a manual user-provided ordering
   */
  public enum VariableOrder {
    FCFS, INORDER, REVERSE, DWAM, MANUAL
  }

  final FactoryType factoryType;
  final VariableOrder variableOrder;

  /**
   * The builder for a BDD configuration.
   */
  public static class Builder {
    private FactoryType factoryType = FactoryType.BUDDY;
    private VariableOrder variableOrder = VariableOrder.FCFS;

    /**
     * Sets the factory type.  The default value is {@code BUDDY}.
     * @param factoryType the factory type
     * @return the builder
     */
    public Builder factoryType(final FactoryType factoryType) {
      this.factoryType = factoryType;
      return this;
    }

    /**
     * Sets the variable order.  The default value is {@code FCFS}.
     * @param variableOrder the variable order
     * @return the builder
     */
    public Builder variableOrder(final VariableOrder variableOrder) {
      this.variableOrder = variableOrder;
      return this;
    }

    /**
     * Builds the BDD configuration.
     * @return the configuration
     */
    public BDDConfig build() {
      return new BDDConfig(this);
    }
  }

  /**
   * Constructs a new BDD configuration.
   * @param builder the builder
   */
  private BDDConfig(final Builder builder) {
    super(ConfigurationType.BDD);
    this.factoryType = builder.factoryType;
    this.variableOrder = builder.variableOrder;
  }

}
