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

package org.logicng.transformations.cnf;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;

/**
 * The configuration object for the CNF encoding.
 * @version 1.2
 * @since 1.1
 */
public final class CNFConfig extends Configuration {

  /**
   * The algorithm for the CNF encoding.
   */
  public enum Algorithm {
    FACTORIZATION, TSEITIN, PLAISTED_GREENBAUM, ADVANCED, BDD
  }

  final Algorithm algorithm;
  final Algorithm fallbackAlgorithmForAdvancedEncoding;
  final int distributionBoundary;
  final int createdClauseBoundary;
  final int atomBoundary;

  /**
   * Constructs a new configuration with a given type.
   * @param builder the builder
   */
  public CNFConfig(final Builder builder) {
    super(ConfigurationType.CNF);
    this.algorithm = builder.algorithm;
    this.fallbackAlgorithmForAdvancedEncoding = builder.fallbackAlgorithmForAdvancedEncoding;
    this.distributionBoundary = builder.distributionBoundary;
    this.createdClauseBoundary = builder.createdClauseBoundary;
    this.atomBoundary = builder.atomBoundary;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder(String.format("CNFConfig{%n"));
    sb.append("algorithm=").append(this.algorithm).append(System.lineSeparator());
    sb.append("fallbackAlgorithmForAdvancedEncoding=").append(this.fallbackAlgorithmForAdvancedEncoding).append(System.lineSeparator());
    sb.append("distributedBoundary=").append(this.distributionBoundary).append(System.lineSeparator());
    sb.append("createdClauseBoundary=").append(this.createdClauseBoundary).append(System.lineSeparator());
    sb.append("atomBoundary=").append(this.atomBoundary).append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  /**
   * The builder for a CNF configuration.
   */
  public static class Builder {

    private Algorithm algorithm = Algorithm.ADVANCED;
    private Algorithm fallbackAlgorithmForAdvancedEncoding = Algorithm.TSEITIN;
    private int distributionBoundary = -1;
    private int createdClauseBoundary = 1000;
    private int atomBoundary = 12;


    /**
     * Sets the algorithm for the CNF encoding. The default value is {@code ADVANCED}.
     * @param algorithm the algorithm for the CNF encoding
     * @return the builder
     */
    public Builder algorithm(final Algorithm algorithm) {
      this.algorithm = algorithm;
      return this;
    }

    /**
     * Sets the fallback algorithm for the advanced CNF encoding.  When the boundaries for the factorization are met, the
     * encoding switches to this algorithm.  The default value is {@code TSEITIN}.
     * @param fallbackAlgorithmForAdvancedEncoding the fallback algorithm for the advanced CNF encoding
     * @return the builder
     */
    public Builder fallbackAlgorithmForAdvancedEncoding(final Algorithm fallbackAlgorithmForAdvancedEncoding) {
      if (fallbackAlgorithmForAdvancedEncoding != Algorithm.TSEITIN
              && fallbackAlgorithmForAdvancedEncoding != Algorithm.PLAISTED_GREENBAUM)
        throw new IllegalArgumentException("Fallback algorithm for advanced CNF encoding must be one of Tseitin or " +
                "Plaisted & Greenbaum");
      this.fallbackAlgorithmForAdvancedEncoding = fallbackAlgorithmForAdvancedEncoding;
      return this;
    }

    /**
     * Sets the boundary how many distributions should be performed in the factorization before the method is switched
     * (in the {@code ADVANCED} encoding).  Disable this boundary by setting it to -1. The default value is -1.
     * @param distributionBoundary the distribution boundary
     * @return the builder
     */
    public Builder distributionBoundary(final int distributionBoundary) {
      this.distributionBoundary = distributionBoundary;
      return this;
    }

    /**
     * Sets the boundary how many clauses should be created in the factorization before the method is switched
     * (in the {@code ADVANCED} encoding).  Disable this boundary by setting it to -1. The default value is 1000.
     * @param createdClauseBoundary the clause creation boundary
     * @return the builder
     */
    public Builder createdClauseBoundary(final int createdClauseBoundary) {
      this.createdClauseBoundary = createdClauseBoundary;
      return this;
    }

    /**
     * Sets the boundary for how many atoms in a formula factorization is performed in Tseitin and Plaisted &amp; Greenbaum.
     * The default value is 12.
     * @param atomBoundary the atom boundary
     * @return the builder
     */
    public Builder atomBoundary(final int atomBoundary) {
      this.atomBoundary = atomBoundary;
      return this;
    }

    /**
     * Builds the configuration.
     * @return the configuration.
     */
    public CNFConfig build() {
      return new CNFConfig(this);
    }
  }
}
