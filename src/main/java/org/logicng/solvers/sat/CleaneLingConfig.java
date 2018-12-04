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

/**
 * The configuration object for a CleaneLing-style SAT solver.
 * @version 1.1
 * @since 1.0
 */
public final class CleaneLingConfig extends Configuration {

  /**
   * The different methods for clause bumping.
   * {@code NONE} - no bumping
   * {@code INC} - number of times a clauses was involved in conflict analysis
   * {@code LRU} - least recently used conflict clauses are kicked out
   * {@code AVG} - running average of most recently used
   */
  public enum ClauseBumping {
    NONE, INC, LRU, AVG
  }

  final boolean block;
  final int blkwait;
  final int blkrtc;
  final int boost;
  final int bwclslim;
  final int bwocclim;
  final ClauseBumping cbump;
  final boolean distill;
  final boolean elim;
  final int elmrtc;
  final int elmocclim;
  final int elmpocclim1;
  final int elmpocclim2;
  final int elmclslim;
  final boolean gluered;
  final int gluekeep;
  final boolean glueupdate;
  final int itsimpdel;
  final boolean plain;
  final boolean restart;
  final int restartint;
  final int redinit;
  final int redinc;
  final boolean reusetrail;
  final int simpint;
  final boolean simpgeom;
  final int sizepen;
  final int sizemaxpen;
  final int searchint;
  final boolean searchfirst;
  final int scincfact;
  final int stepslim;

  /**
   * Constructs a new CleaneLing configuration from a given builder.
   * @param builder the builder
   */
  private CleaneLingConfig(final Builder builder) {
    super(ConfigurationType.CLEANELING);
    this.block = builder.blockedClauseElimination;
    this.blkwait = builder.blockedClauseEliminationWait;
    this.blkrtc = builder.blockedClauseEliminationRTC;
    this.boost = builder.boost;
    this.bwclslim = builder.bwClauseLim;
    this.bwocclim = builder.bwOccurrenceLim;
    this.cbump = builder.clauseBumping;
    this.distill = builder.distillation;
    this.elim = builder.bvElim;
    this.elmrtc = builder.bvElimRTC;
    this.elmocclim = builder.bvElimOccurrenceLim;
    this.elmpocclim1 = builder.bvElimPivotOccurrenceLimOneSided;
    this.elmpocclim2 = builder.bvElimPivotOccurrenceLimTwoSided;
    this.elmclslim = builder.bvElimClauseLim;
    this.gluered = builder.gluered;
    this.gluekeep = builder.glueKeep;
    this.glueupdate = builder.glueUpdate;
    this.itsimpdel = builder.iterationSimplificationDelay;
    this.plain = builder.plain;
    this.restart = builder.restart;
    this.restartint = builder.restartInterval;
    this.redinit = builder.reductionInterval;
    this.redinc = builder.reductionIntervalInc;
    this.reusetrail = builder.reuseTrail;
    this.simpint = builder.simpSteps;
    this.simpgeom = builder.simpGeomIncrease;
    this.sizepen = builder.sizePenalty;
    this.sizemaxpen = builder.sizeMaxPenalty;
    this.searchint = builder.searchInterval;
    this.searchfirst = builder.searchFirst;
    this.scincfact = builder.scoreIncrementFactor;
    this.stepslim = builder.stepsLim;
  }

  /**
   * Returns whether the solver is in 'plain' mode or not.
   * @return {@code true} if the solver is in plain mode, {@code false} otherwise
   */
  public boolean plain() {
    return this.plain;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("CleaneLingConfig{").append(System.lineSeparator());
    sb.append("blockedClauseElimination=").append(this.block).append(System.lineSeparator());
    sb.append("blockedClauseEliminationWait=").append(this.blkwait).append(System.lineSeparator());
    sb.append("blockedClauseEliminationRTC=").append(this.blkrtc).append(System.lineSeparator());
    sb.append("boost=").append(this.boost).append(System.lineSeparator());
    sb.append("bwClauseLim=").append(this.bwclslim).append(System.lineSeparator());
    sb.append("bwOccurrenceLim=").append(this.bwocclim).append(System.lineSeparator());
    sb.append("clauseBumping=").append(this.cbump).append(System.lineSeparator());
    sb.append("distillation=").append(this.distill).append(System.lineSeparator());
    sb.append("bvElim=").append(this.elim).append(System.lineSeparator());
    sb.append("bvElimRTC=").append(this.elmrtc).append(System.lineSeparator());
    sb.append("bvElimOccurrenceLim=").append(this.elmocclim).append(System.lineSeparator());
    sb.append("bvElimPivotOccurrenceLimOneSided=").append(this.elmpocclim1).append(System.lineSeparator());
    sb.append("bvElimPivotOccurrenceLimTwoSided=").append(this.elmpocclim2).append(System.lineSeparator());
    sb.append("bvElimClauseLim=").append(this.elmclslim).append(System.lineSeparator());
    sb.append("gluered=").append(this.gluered).append(System.lineSeparator());
    sb.append("glueKeep=").append(this.gluekeep).append(System.lineSeparator());
    sb.append("glueUpdate=").append(this.glueupdate).append(System.lineSeparator());
    sb.append("iterationSimplificationDelay=").append(this.itsimpdel).append(System.lineSeparator());
    sb.append("plain=").append(this.plain).append(System.lineSeparator());
    sb.append("restart=").append(this.restart).append(System.lineSeparator());
    sb.append("restartInterval=").append(this.restartint).append(System.lineSeparator());
    sb.append("reductionInterval=").append(this.redinit).append(System.lineSeparator());
    sb.append("reductionIntervalInc=").append(this.redinc).append(System.lineSeparator());
    sb.append("reuseTrail=").append(this.reusetrail).append(System.lineSeparator());
    sb.append("simpSteps=").append(this.simpint).append(System.lineSeparator());
    sb.append("simpGeomIncrease=").append(this.simpgeom).append(System.lineSeparator());
    sb.append("sizePenalty=").append(this.sizepen).append(System.lineSeparator());
    sb.append("sizeMaxPenalty=").append(this.sizemaxpen).append(System.lineSeparator());
    sb.append("searchInterval=").append(this.searchint).append(System.lineSeparator());
    sb.append("searchFirst=").append(this.searchfirst).append(System.lineSeparator());
    sb.append("scoreIncrementFactor=").append(this.scincfact).append(System.lineSeparator());
    sb.append("stepsLim=").append(this.stepslim).append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  /**
   * The builder for a MiniSAT configuration.
   */
  public static class Builder {
    private boolean blockedClauseElimination = true;
    private int blockedClauseEliminationWait = 1;
    private int blockedClauseEliminationRTC = 0;
    private int boost = 10;
    private int bwClauseLim = 10000;
    private int bwOccurrenceLim = 10000;
    private ClauseBumping clauseBumping = ClauseBumping.INC;
    private boolean distillation = true;
    private boolean bvElim = true;
    private int bvElimRTC = 0;
    private int bvElimOccurrenceLim = 10000;
    private int bvElimPivotOccurrenceLimOneSided = 100;
    private int bvElimPivotOccurrenceLimTwoSided = 8;
    private int bvElimClauseLim = 1000;
    private boolean gluered = true;
    private int glueKeep = 1;
    private boolean glueUpdate = false;
    private int iterationSimplificationDelay = 10;
    private boolean plain = false;
    private boolean restart = true;
    private int restartInterval = 100;
    private int reductionInterval = 1000;
    private int reductionIntervalInc = 2000;
    private boolean reuseTrail = true;
    private int simpSteps = 2000000;
    private boolean simpGeomIncrease = true;
    private int sizePenalty = 1 << 17;
    private int sizeMaxPenalty = 5;
    private int searchInterval = 5000;
    private boolean searchFirst = false;
    private int scoreIncrementFactor = 1050;
    private int stepsLim = 100000000;

    /**
     * If turned on, blocked clause elimination (BCE) will be performed.  The default value is {@code true}.
     * @param blockedClauseElimination {@code true} if BCE should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder blockedClauseElimination(boolean blockedClauseElimination) {
      this.blockedClauseElimination = blockedClauseElimination;
      return this;
    }

    /**
     * Sets the number of simplification steps before BCE is performed.  The default value is 1.
     * @param blockedClauseEliminationWait the number of simplification steps before BCE is performed
     * @return the builder
     */
    public Builder blockedClauseEliminationWait(int blockedClauseEliminationWait) {
      this.blockedClauseEliminationWait = blockedClauseEliminationWait;
      return this;
    }

    /**
     * Sets the number of simplification steps before BCE is run to completion.  The default value is 0.
     * @param blockedClauseEliminationRTC the number of simplification steps before BCE is run to completion
     * @return the builder
     */
    public Builder blockedClauseEliminationRTC(int blockedClauseEliminationRTC) {
      this.blockedClauseEliminationRTC = blockedClauseEliminationRTC;
      return this;
    }

    /**
     * Sets the initial effort for simplification.  The default value is 10.
     * @param boost the initial effort for simplification
     * @return the builder
     */
    public Builder boost(int boost) {
      this.boost = boost;
      return this;
    }

    /**
     * Sets the backward-subsumption clause size limit.  The default value is 10,000.
     * @param bwClauseLim the backward-subsumption clause size limit
     * @return the builder
     */
    public Builder bwClauseLim(int bwClauseLim) {
      this.bwClauseLim = bwClauseLim;
      return this;
    }

    /**
     * Sets the backward-subsumption occurrence limit.  The default value is 10,000.
     * @param bwOccurrenceLim the backward-subsumption occurrence limit
     * @return the builder
     */
    public Builder bwOccurrenceLim(int bwOccurrenceLim) {
      this.bwOccurrenceLim = bwOccurrenceLim;
      return this;
    }

    /**
     * Sets the method for clause bumping.  The default value ist {@code INC}.
     * @param clauseBumping the method for clause bumping
     * @return the builder
     */
    public Builder clauseBumping(final ClauseBumping clauseBumping) {
      this.clauseBumping = clauseBumping;
      return this;
    }

    /**
     * If turned on, distillation will be performed.  The default value is {@code true}.
     * @param distillation {@code true} if distillation should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder distillation(boolean distillation) {
      this.distillation = distillation;
      return this;
    }

    /**
     * If turned on, bounded variable elimination (BVE) will be performed.  The default value is {@code true}.
     * @param bvElim {@code true} if bounded variable elimination (BVE) should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder bvElim(boolean bvElim) {
      this.bvElim = bvElim;
      return this;
    }

    /**
     * Sets the number of simplification steps before BVE is run to completion.  The default value is 0.
     * @param bvElimRTC the number of simplification steps before BVE is run to completion
     * @return the builder
     */
    public Builder bvElimRTC(int bvElimRTC) {
      this.bvElimRTC = bvElimRTC;
      return this;
    }

    /**
     * Sets the number of maximum occurrences in BVE.  The default value is 10,000.
     * @param bvElimOccurrenceLim the number of maximum occurrences in BVE
     * @return the builder
     */
    public Builder bvElimOccurrenceLim(int bvElimOccurrenceLim) {
      this.bvElimOccurrenceLim = bvElimOccurrenceLim;
      return this;
    }

    /**
     * Sets the number of maximum one-sided occurrences of BVE pivot.  The default value is 100.
     * @param bvElimPivotOccurrenceLimOneSided the number of maximum one-sided occurrences of BVE pivot
     * @return the builder
     */
    public Builder bvElimPivotOccurrenceLimOneSided(int bvElimPivotOccurrenceLimOneSided) {
      this.bvElimPivotOccurrenceLimOneSided = bvElimPivotOccurrenceLimOneSided;
      return this;
    }

    /**
     * Sets the number of maximum two-sided occurrences of BVE pivot.  The default value is 8.
     * @param bvElimPivotOccurrenceLimTwoSided the number of maximum two-sided occurrences of BVE pivot
     * @return the builder
     */
    public Builder bvElimPivotOccurrenceLimTwoSided(int bvElimPivotOccurrenceLimTwoSided) {
      this.bvElimPivotOccurrenceLimTwoSided = bvElimPivotOccurrenceLimTwoSided;
      return this;
    }

    /**
     * Sets the maximum antecedent size in BVE.  The default value is 1000.
     * @param bvElimClauseLim the maximum antecedent size in BVE
     * @return the builder
     */
    public Builder bvElimClauseLim(int bvElimClauseLim) {
      this.bvElimClauseLim = bvElimClauseLim;
      return this;
    }

    /**
     * If turned on, glue (LBD) based reduction will be performed.  The default value is {@code true}.
     * @param gluered {@code true} if glue (LBD) based reduction should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder gluered(boolean gluered) {
      this.gluered = gluered;
      return this;
    }

    /**
     * Sets the glue of which size clauses are kept.  The default value is 1.
     * @param glueKeep the glue of which size clauses are kept
     * @return the builder
     */
    public Builder glueKeep(int glueKeep) {
      this.glueKeep = glueKeep;
      return this;
    }

    /**
     * If turned on, glue updates be performed.  The default value is {@code true}.
     * @param glueUpdate {@code true} if glue updates should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder glueUpdate(boolean glueUpdate) {
      this.glueUpdate = glueUpdate;
      return this;
    }

    /**
     * Sets the iteration simplification delay in conflicts.  The default value is 10.
     * @param iterationSimplificationDelay the iteration simplification delay in conflicts
     * @return the builder
     */
    public Builder iterationSimplificationDelay(int iterationSimplificationDelay) {
      this.iterationSimplificationDelay = iterationSimplificationDelay;
      return this;
    }

    /**
     * If turned on, plain solving (no inprocessing/preprocessing) is performed.  The default value is {@code false}.
     * @param plain {@code true} if plain solving should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder plain(boolean plain) {
      this.plain = plain;
      return this;
    }

    /**
     * If turned on, restarts are enabled.  The default value is {@code true}.
     * @param restart {@code true} if restarts are enabled, {@code false} otherwise
     * @return the builder
     */
    public Builder restart(boolean restart) {
      this.restart = restart;
      return this;
    }

    /**
     * Sets the basic restart interval.  The default value is 100.
     * @param restartInterval the basic restart interval
     * @return the builder
     */
    public Builder restartInterval(int restartInterval) {
      this.restartInterval = restartInterval;
      return this;
    }

    /**
     * Sets the initial reduction interval.  The default value is 1000.
     * @param reductionInterval the initial reduction interval
     * @return the builder
     */
    public Builder reductionInterval(int reductionInterval) {
      this.reductionInterval = reductionInterval;
      return this;
    }

    /**
     * Sets the reduction interval increment.  The default value is 2000.
     * @param reductionIntervalInc the reduction interval increment
     * @return the builder
     */
    public Builder reductionIntervalInc(int reductionIntervalInc) {
      this.reductionIntervalInc = reductionIntervalInc;
      return this;
    }


    /**
     * If turned on, the trail will be reused.  The default value is {@code true}.
     * @param reuseTrail {@code true} if the trail should be reused, {@code false} otherwise
     * @return the builder
     */
    public Builder reuseTrail(boolean reuseTrail) {
      this.reuseTrail = reuseTrail;
      return this;
    }

    /**
     * Sets the inprocessing steps interval.  The default value is 2,000,000.
     * @param simpSteps the inprocessing steps interval
     * @return the builder
     */
    public Builder simpSteps(int simpSteps) {
      this.simpSteps = simpSteps;
      return this;
    }

    /**
     * If turned on, geometric increase in simplification effort will be performed.  The default value is {@code true}.
     * @param simpGeomIncrease {@code true} if geometric increase in simplification effort should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder simpGeomIncrease(boolean simpGeomIncrease) {
      this.simpGeomIncrease = simpGeomIncrease;
      return this;
    }

    /**
     * Sets the size penalty for the number of clauses.  The default value is 1 &lt;&lt; 17.
     * @param sizePenalty the size penalty for the number of clauses
     * @return the builder
     */
    public Builder sizePenalty(int sizePenalty) {
      this.sizePenalty = sizePenalty;
      return this;
    }

    /**
     * Sets the maximum logarithmic size penalty.  The default value is 5.
     * @param sizeMaxPenalty the maximum logarithmic size penalty
     * @return the builder
     */
    public Builder sizeMaxPenalty(int sizeMaxPenalty) {
      this.sizeMaxPenalty = sizeMaxPenalty;
      return this;
    }

    /**
     * Sets the CDCL search conflict interval.  The default value is 5,000.
     * @param searchInterval the CDCL search conflict interval
     * @return the builder
     */
    public Builder searchInterval(int searchInterval) {
      this.searchInterval = searchInterval;
      return this;
    }

    /**
     * If turned on, search is first performed instead of simplifying first.  The default value is {@code true}.
     * @param searchFirst {@code true} if search should be first performed instead of simplifying, {@code false} otherwise
     * @return the builder
     */
    public Builder searchFirst(boolean searchFirst) {
      this.searchFirst = searchFirst;
      return this;
    }

    /**
     * Sets the variable score increment in per mille.  The default value is 1050.
     * @param scoreIncrementFactor the variable score increment in per mille
     * @return the builder
     */
    public Builder scoreIncrementFactor(int scoreIncrementFactor) {
      this.scoreIncrementFactor = scoreIncrementFactor;
      return this;
    }

    /**
     * Sets the maximum steps limit (0=none).  The default value is 100,000,000.
     * @param stepsLim maximum steps limit
     * @return the builder
     */
    public Builder stepsLim(int stepsLim) {
      this.stepsLim = stepsLim;
      return this;
    }

    /**
     * Builds the CleaneLing configuration.
     * @return the configuration
     */
    public CleaneLingConfig build() {
      return new CleaneLingConfig(this);
    }

  }
}
