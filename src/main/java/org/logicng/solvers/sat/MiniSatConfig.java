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

package org.logicng.solvers.sat;

import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;

import static org.logicng.solvers.sat.MiniSatConfig.ClauseMinimization.DEEP;

/**
 * The configuration object for a MiniSAT-style SAT solver.
 * @version 1.3
 * @since 1.0
 */
public final class MiniSatConfig extends Configuration {

  /**
   * The different methods for clause minimization.
   * {@code NONE} - no minimization is performed
   * {@code BASIC} - local minimization is performed
   * {@code DEEP} - recursive minimization is performed
   */
  public enum ClauseMinimization {
    NONE, BASIC, DEEP
  }

  final double varDecay;
  final double varInc;
  final ClauseMinimization clauseMin;
  final int restartFirst;
  final double restartInc;
  final double clauseDecay;
  final boolean removeSatisfied;
  final double learntsizeFactor;
  final double learntsizeInc;
  final boolean incremental;
  final boolean initialPhase;
  final boolean proofGeneration;

  /**
   * Constructs a new MiniSAT configuration from a given builder.
   * @param builder the builder
   */
  private MiniSatConfig(final Builder builder) {
    super(ConfigurationType.MINISAT);
    this.varDecay = builder.varDecay;
    this.varInc = builder.varInc;
    this.clauseMin = builder.clauseMin;
    this.restartFirst = builder.restartFirst;
    this.restartInc = builder.restartInc;
    this.clauseDecay = builder.clauseDecay;
    this.removeSatisfied = builder.removeSatisfied;
    this.learntsizeFactor = builder.learntsizeFactor;
    this.learntsizeInc = builder.learntsizeInc;
    this.incremental = builder.incremental;
    this.initialPhase = builder.initialPhase;
    this.proofGeneration = builder.proofGeneration;
  }

  /**
   * Returns whether the solver is incremental or not
   * @return {@code true} if the solver is incremental, {@code false} otherwise
   */
  public boolean incremental() {
    return this.incremental;
  }

  /**
   * Returns the initial phase of the solver.
   * @return the initial phase of the solver
   */
  public boolean initialPhase() {
    return this.initialPhase;
  }

  /**
   * Returns whether proof generation should be performed or not.
   * @return whether proof generation should be performed or not
   */
  public boolean proofGeneration() {
    return this.proofGeneration;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("MiniSatConfig{").append(System.lineSeparator());
    sb.append("varDecay=").append(this.varDecay).append(System.lineSeparator());
    sb.append("varInc=").append(this.varInc).append(System.lineSeparator());
    sb.append("clauseMin=").append(this.clauseMin).append(System.lineSeparator());
    sb.append("restartFirst=").append(this.restartFirst).append(System.lineSeparator());
    sb.append("restartInc=").append(this.restartInc).append(System.lineSeparator());
    sb.append("clauseDecay=").append(this.clauseDecay).append(System.lineSeparator());
    sb.append("removeSatisfied=").append(this.removeSatisfied).append(System.lineSeparator());
    sb.append("learntsizeFactor=").append(this.learntsizeFactor).append(System.lineSeparator());
    sb.append("learntsizeInc=").append(this.learntsizeInc).append(System.lineSeparator());
    sb.append("incremental=").append(this.incremental).append(System.lineSeparator());
    sb.append("initialPhase=").append(this.initialPhase).append(System.lineSeparator());
    sb.append("proofGeneration=").append(this.proofGeneration).append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  /**
   * The builder for a MiniSAT configuration.
   */
  public static class Builder {
    private double varDecay = 0.95;
    private double varInc = 1.0;
    private ClauseMinimization clauseMin = DEEP;
    private int restartFirst = 100;
    private double restartInc = 2.0;
    private double clauseDecay = 0.999;
    private boolean removeSatisfied = true;
    private double learntsizeFactor = 1.0 / 3.0;
    private double learntsizeInc = 1.1;
    private boolean incremental = true;
    private boolean initialPhase = false;
    private boolean proofGeneration = false;

    /**
     * Sets the variable activity decay factor to a given value. The default value is 0.95.
     * @param varDecay the value (should be in the range 0..1)
     * @return the builder
     */
    public Builder varDecay(double varDecay) {
      this.varDecay = varDecay;
      return this;
    }

    /**
     * Sets the initial value to bump a variable with each time it is used in conflict resolution to a given value.
     * The default value is 1.0.
     * @param varInc the value
     * @return the builder
     */
    public Builder varInc(double varInc) {
      this.varInc = varInc;
      return this;
    }

    /**
     * Sets the clause minimization method. The default value is {@code DEEP}.
     * @param ccmin the value
     * @return the builder
     */
    public Builder clMinimization(final ClauseMinimization ccmin) {
      this.clauseMin = ccmin;
      return this;
    }

    /**
     * Sets the base restart interval to the given value. The default value is 100.
     * @param restartFirst the value (should be at least 1)
     * @return the builder
     */
    public Builder restartFirst(int restartFirst) {
      this.restartFirst = restartFirst;
      return this;
    }

    /**
     * Sets the restart interval increase factor to the given value. The default value is 2.0.
     * @param restartInc the value (should be at least 1)
     * @return the builder
     */
    public Builder restartInc(double restartInc) {
      this.restartInc = restartInc;
      return this;
    }

    /**
     * Sets the clause activity decay factor to a given value. The default value is 0.999.
     * @param clauseDecay the value (should be in the range 0..1)
     * @return the builder
     */
    public Builder clauseDecay(double clauseDecay) {
      this.clauseDecay = clauseDecay;
      return this;
    }

    /**
     * If turned on, the satisfied original clauses will be removed when simplifying on level 0, when turned off,
     * only the satisfied learnt clauses will be removed.  The default value is {@code true}.
     * @param removeSatisfied {@code true} if the original clauses should be simplified, {@code false} otherwise
     * @return the builder
     */
    public Builder removeSatisfied(boolean removeSatisfied) {
      this.removeSatisfied = removeSatisfied;
      return this;
    }

    /**
     * Sets the initial limit for learnt clauses as a factor of the original clauses to the given value.  The default
     * value is 1/3.
     * @param learntsizeFactor the value
     * @return the builder
     */
    public Builder lsFactor(double learntsizeFactor) {
      this.learntsizeFactor = learntsizeFactor;
      return this;
    }

    /**
     * Sets the factor by which the limit for learnt clauses is multiplied every restart to a given value. The default
     * value is 1.1.
     * @param learntsizeInc the value
     * @return the builder
     */
    public Builder lsInc(double learntsizeInc) {
      this.learntsizeInc = learntsizeInc;
      return this;
    }

    /**
     * Turns the incremental mode of the solver off and on.  The default value is {@code true}.
     * @param incremental {@code true} if incremental mode is turned on, {@code false} otherwise
     * @return the builder
     */
    public Builder incremental(boolean incremental) {
      this.incremental = incremental;
      return this;
    }

    /**
     * Sets the initial phase of the solver.  The default value is {@code true}.
     * @param initialPhase the initial phase
     * @return the builder
     */
    public Builder initialPhase(boolean initialPhase) {
      this.initialPhase = initialPhase;
      return this;
    }

    /**
     * Sets whether the information for generating a proof with DRUP should be recorded or not.
     * @param proofGeneration {@code true} if proof generating information should be recorded, {@code false} otherwise
     * @return the builder
     */
    public Builder proofGeneration(boolean proofGeneration) {
      this.proofGeneration = proofGeneration;
      return this;
    }

    /**
     * Builds the MiniSAT configuration.
     * @return the configuration
     */
    public MiniSatConfig build() {
      return new MiniSatConfig(this);
    }
  }
}
