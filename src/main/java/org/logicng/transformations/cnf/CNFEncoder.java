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
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.handlers.FactorizationHandler;

import java.util.ArrayList;
import java.util.List;

/**
 * An encoder for conjunctive normal form (CNF).
 * @version 1.1
 * @since 1.1
 */
public class CNFEncoder {

  private final FormulaFactory f;
  private final CNFConfig config;
  private final CNFConfig defaultConfig;

  private CNFFactorization factorization;
  private CNFFactorization advancedFactorization;
  private BDDCNFTransformation bddCnfTransformation;
  private TseitinTransformation tseitin;
  private PlaistedGreenbaumTransformation plaistedGreenbaum;
  private int currentAtomBoundary;
  private AdvancedFactorizationHandler factorizationHandler;

  /**
   * Constructs a new CNF encoder with a given configuration.
   * @param f      the formula factory
   * @param config the configuration
   */
  public CNFEncoder(final FormulaFactory f, final CNFConfig config) {
    this.f = f;
    this.config = config;
    this.defaultConfig = new CNFConfig.Builder().build();
  }

  /**
   * Constructs a new CNF encoder which uses the configuration of the formula factory.
   * @param f the formula factory
   */
  public CNFEncoder(final FormulaFactory f) {
    this(f, null);
  }

  /**
   * Encodes a formula to CNF.
   * @param formula formula
   * @return the CNF encoding of the formula
   */
  public Formula encode(final Formula formula) {
    switch (this.config().algorithm) {
      case FACTORIZATION:
        if (this.factorization == null)
          this.factorization = new CNFFactorization();
        return formula.transform(this.factorization);
      case TSEITIN:
        if (this.tseitin == null || this.currentAtomBoundary != this.config().atomBoundary) {
          this.currentAtomBoundary = this.config().atomBoundary;
          this.tseitin = new TseitinTransformation(this.config().atomBoundary);
        }
        return formula.transform(this.tseitin);
      case PLAISTED_GREENBAUM:
        if (this.plaistedGreenbaum == null || this.currentAtomBoundary != this.config().atomBoundary) {
          this.currentAtomBoundary = this.config().atomBoundary;
          this.plaistedGreenbaum = new PlaistedGreenbaumTransformation(this.config().atomBoundary);
        }
        return formula.transform(this.plaistedGreenbaum);
      case BDD:
        if (this.bddCnfTransformation == null)
          this.bddCnfTransformation = new BDDCNFTransformation();
        return formula.transform(this.bddCnfTransformation);
      case ADVANCED:
        if (this.factorizationHandler == null) {
          this.factorizationHandler = new AdvancedFactorizationHandler();
          this.advancedFactorization = new CNFFactorization(this.factorizationHandler);
        }
        this.factorizationHandler.reset(this.config().distributionBoundary, this.config().createdClauseBoundary);
        return this.advancedEncoding(formula);
      default:
        throw new IllegalStateException("Unknown CNF encoding algorithm: " + this.config().algorithm);
    }
  }

  /**
   * Encodes the given formula to CNF by first trying to use Factorization for the single sub-formulas.  When certain
   * user-provided boundaries are met, the method is switched to Tseitin or Plaisted & Greenbaum.
   * @param formula the formula
   * @return the CNF encoding of the formula
   */
  private Formula advancedEncoding(final Formula formula) {
    if (formula.type() == FType.AND) {
      final List<Formula> operands = new ArrayList<>(formula.numberOfOperands());
      for (final Formula op : formula)
        operands.add(singleAdvancedEncoding(op));
      return this.f.and(operands);
    }
    return singleAdvancedEncoding(formula);
  }

  private Formula singleAdvancedEncoding(final Formula formula) {
    Formula result = formula.transform(this.advancedFactorization);
    if (result == null)
      switch (this.config().fallbackAlgorithmForAdvancedEncoding) {
        case TSEITIN:
          if (this.tseitin == null || this.currentAtomBoundary != this.config().atomBoundary) {
            this.currentAtomBoundary = this.config().atomBoundary;
            this.tseitin = new TseitinTransformation(this.config().atomBoundary);
          }
          result = formula.transform(this.tseitin);
          break;
        case PLAISTED_GREENBAUM:
          if (this.plaistedGreenbaum == null || this.currentAtomBoundary != this.config().atomBoundary) {
            this.currentAtomBoundary = this.config().atomBoundary;
            this.plaistedGreenbaum = new PlaistedGreenbaumTransformation(this.config().atomBoundary);
          }
          result = formula.transform(this.plaistedGreenbaum);
          break;
        default:
          throw new IllegalStateException("Invalid fallback CNF encoding algorithm: " + this.config().fallbackAlgorithmForAdvancedEncoding);
      }
    return result;
  }

  /**
   * Returns the current configuration of this encoder.  If the encoder was constructed with a given configuration, this
   * configuration will always be used.  Otherwise the current configuration of the formula factory is used or - if not
   * present - the default configuration.
   * @return the current configuration of
   */
  public CNFConfig config() {
    if (this.config != null)
      return this.config;
    final Configuration cnfConfig = this.f.configurationFor(ConfigurationType.CNF);
    return cnfConfig != null ? (CNFConfig) cnfConfig : this.defaultConfig;
  }

  @Override
  public String toString() {
    return this.config().toString();
  }

  /**
   * The factorization handler for the advanced CNF encoding.
   */
  private static class AdvancedFactorizationHandler implements FactorizationHandler {

    private int distributionBoundary;
    private int createdClauseBoundary;
    private int currentDistributions;
    private int currentClauses;

    private void reset(final int distributionBoundary, final int createdClauseBoundary) {
      this.distributionBoundary = distributionBoundary;
      this.createdClauseBoundary = createdClauseBoundary;
      this.currentDistributions = 0;
      this.currentClauses = 0;
    }

    @Override
    public boolean performedDistribution() {
      return this.distributionBoundary == -1 || ++this.currentDistributions <= this.distributionBoundary;
    }

    @Override
    public boolean createdClause(final Formula clause) {
      return this.createdClauseBoundary == -1 || ++this.currentClauses <= this.createdClauseBoundary;
    }
  }
}
