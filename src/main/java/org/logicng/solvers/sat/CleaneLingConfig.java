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
//  Copyright 2015 Christoph Zengler                                     //
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
 * @author Christoph Zengler
 * @version 1.0
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
   * The builder for a MiniSAT configuration.
   */
  public static class Builder {
    private boolean block = true;
    private int blkwait = 1;
    private int blkrtc = 0;
    private int boost = 10;
    private int bwclslim = 10000;
    private int bwocclim = 10000;
    private ClauseBumping cbump = ClauseBumping.INC;
    private boolean distill = true;
    private boolean elim = true;
    private int elmrtc = 0;
    private int elmocclim = 10000;
    private int elmpocclim1 = 100;
    private int elmpocclim2 = 8;
    private int elmclslim = 1000;
    private boolean gluered = true;
    private int gluekeep = 1;
    private boolean glueupdate = false;
    private int itsimpdel = 10;
    private boolean plain = false;
    private boolean restart = true;
    private int restartint = 100;
    private int redinit = 1000;
    private int redinc = 2000;
    private boolean reusetrail = true;
    private int simpint = 2000000;
    private boolean simpgeom = true;
    private int sizepen = 1 << 17;
    private int sizemaxpen = 5;
    private int searchint = 5000;
    private boolean searchfirst = false;
    private int scincfact = 1050;
    private int stepslim = 100000000;

    /**
     * If turned on, blocked clause elimination (BCE) will be performed.  The default value is {@code true}.
     * @param bce {@code true} if BCE should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder bce(boolean bce) {
      this.block = bce;
      return this;
    }

    /**
     * Sets the number of simplification steps before BCE is performed.  The default value is 1.
     * @param blkwait the number of simplification steps before BCE is performed
     * @return the builder
     */
    public Builder blkwait(int blkwait) {
      this.blkwait = blkwait;
      return this;
    }

    /**
     * Sets the number of simplification steps before BCE is run to completion.  The default value is 0.
     * @param blkrtc the number of simplification steps before BCE is run to completion
     * @return the builder
     */
    public Builder blkrtc(int blkrtc) {
      this.blkrtc = blkrtc;
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
     * @param bwclslim the backward-subsumption clause size limit
     * @return the builder
     */
    public Builder bwclslim(int bwclslim) {
      this.bwclslim = bwclslim;
      return this;
    }

    /**
     * Sets the backward-subsumption occurrence limit.  The default value is 10,000.
     * @param bwocclim the backward-subsumption occurrence limit
     * @return the builder
     */
    public Builder bwocclim(int bwocclim) {
      this.bwocclim = bwocclim;
      return this;
    }

    /**
     * Sets the method for clause bumping.  The default value ist {@code INT}.
     * @param cbump the method for clause bumping
     * @return the builder
     */
    public Builder cbump(final ClauseBumping cbump) {
      this.cbump = cbump;
      return this;
    }

    /**
     * If turned on, distillation will be performed.  The default value is {@code true}.
     * @param distill {@code true} if distillation should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder distill(boolean distill) {
      this.distill = distill;
      return this;
    }

    /**
     * If turned on, bounded variable elimination (BVE) will be performed.  The default value is {@code true}.
     * @param elim {@code true} if bounded variable elimination (BVE) should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder elim(boolean elim) {
      this.elim = elim;
      return this;
    }

    /**
     * Sets the number of simplification steps before BVE is run to completion.  The default value is 0.
     * @param elmrtc the number of simplification steps before BVE is run to completion
     * @return the builder
     */
    public Builder elmrtc(int elmrtc) {
      this.elmrtc = elmrtc;
      return this;
    }

    /**
     * Sets the number of maximum occurrences in BVE.  The default value is 10,000.
     * @param elmocclim the number of maximum occurrences in BVE
     * @return the builder
     */
    public Builder elmocclim(int elmocclim) {
      this.elmocclim = elmocclim;
      return this;
    }

    /**
     * Sets the number of maximum one-sided occurrences of BVE pivot.  The default value is 100.
     * @param elmpocclim1 the number of maximum one-sided occurrences of BVE pivot
     * @return the builder
     */
    public Builder elmpocclim1(int elmpocclim1) {
      this.elmpocclim1 = elmpocclim1;
      return this;
    }

    /**
     * Sets the number of maximum two-sided occurrences of BVE pivot.  The default value is 8.
     * @param elmpocclim2 the number of maximum two-sided occurrences of BVE pivot
     * @return the builder
     */
    public Builder elmpocclim2(int elmpocclim2) {
      this.elmpocclim2 = elmpocclim2;
      return this;
    }

    /**
     * Sets the maximum antecedent size in BVE.  The default value is 1000.
     * @param elmclslim the maximum antecedent size in BVE
     * @return the builder
     */
    public Builder elmclslim(int elmclslim) {
      this.elmclslim = elmclslim;
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
     * @param gluekeep the glue of which size clauses are kept
     * @return the builder
     */
    public Builder gluekeep(int gluekeep) {
      this.gluekeep = gluekeep;
      return this;
    }

    /**
     * If turned on, glue updates be performed.  The default value is {@code true}.
     * @param glueupdate {@code true} if glue updates should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder glueupdate(boolean glueupdate) {
      this.glueupdate = glueupdate;
      return this;
    }

    /**
     * Sets the iteration simplification delay in conflicts.  The default value is 10.
     * @param itsimpdel the iteration simplification delay in conflicts
     * @return the builder
     */
    public Builder itsimpdel(int itsimpdel) {
      this.itsimpdel = itsimpdel;
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
     * @param restartint the basic restart interval
     * @return the builder
     */
    public Builder restartint(int restartint) {
      this.restartint = restartint;
      return this;
    }

    /**
     * Sets the initial reduction interval.  The default value is 1000.
     * @param redinit the initial reduction interval
     * @return the builder
     */
    public Builder redinit(int redinit) {
      this.redinit = redinit;
      return this;
    }

    /**
     * Sets the reduction interval increment.  The default value is 2000.
     * @param redinc the reduction interval increment
     * @return the builder
     */
    public Builder redinc(int redinc) {
      this.redinc = redinc;
      return this;
    }


    /**
     * If turned on, the trail will be reused.  The default value is {@code true}.
     * @param reusetrail {@code true} if the trail should be reused, {@code false} otherwise
     * @return the builder
     */
    public Builder reusetrail(boolean reusetrail) {
      this.reusetrail = reusetrail;
      return this;
    }

    /**
     * Sets the inprocessing steps interval.  The default value is 2,000,000.
     * @param simpint the inprocessing steps interval
     * @return the builder
     */
    public Builder simpint(int simpint) {
      this.simpint = simpint;
      return this;
    }

    /**
     * If turned on, geometric increase in simplification effort will be performed.  The default value is {@code true}.
     * @param simpgeom {@code true} if geometric increase in simplification effort should be performed, {@code false} otherwise
     * @return the builder
     */
    public Builder simpgeom(boolean simpgeom) {
      this.simpgeom = simpgeom;
      return this;
    }

    /**
     * Sets the size penalty for the number of clauses.  The default value is 1 &lt;&lt; 17.
     * @param sizepen the size penalty for the number of clauses
     * @return the builder
     */
    public Builder sizepen(int sizepen) {
      this.sizepen = sizepen;
      return this;
    }

    /**
     * Sets the maximum logarithmic size penalty.  The default value is 5.
     * @param sizemaxpen the maximum logarithmic size penalty
     * @return the builder
     */
    public Builder sizemaxpen(int sizemaxpen) {
      this.sizemaxpen = sizemaxpen;
      return this;
    }

    /**
     * Sets the CDCL search conflict interval.  The default value is 5,000.
     * @param searchint the CDCL search conflict interval
     * @return the builder
     */
    public Builder searchint(int searchint) {
      this.searchint = searchint;
      return this;
    }

    /**
     * If turned on, search is first performed instead of simplifying first.  The default value is {@code true}.
     * @param searchfirst {@code true} if search sould be first performed instead of simplifying, {@code false} otherwise
     * @return the builder
     */
    public Builder searchfirst(boolean searchfirst) {
      this.searchfirst = searchfirst;
      return this;
    }

    /**
     * Sets the variable score increment in per mille.  The default value is 1050.
     * @param scincfact the variable score increment in per mille
     * @return the builder
     */
    public Builder scincfact(int scincfact) {
      this.scincfact = scincfact;
      return this;
    }

    /**
     * Sets the maximum steps limit (0=none).  The default value is 100,000,000.
     * @param stepslim maximum steps limit
     * @return the builder
     */
    public Builder stepslim(int stepslim) {
      this.stepslim = stepslim;
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

  /**
   * Constructs a new CleaneLing configuration from a given builder.
   * @param builder the builder
   */
  private CleaneLingConfig(final Builder builder) {
    super(ConfigurationType.CLEANELING);
    this.block = builder.block;
    this.blkwait = builder.blkwait;
    this.blkrtc = builder.blkrtc;
    this.boost = builder.boost;
    this.bwclslim = builder.bwclslim;
    this.bwocclim = builder.bwocclim;
    this.cbump = builder.cbump;
    this.distill = builder.distill;
    this.elim = builder.elim;
    this.elmrtc = builder.elmrtc;
    this.elmocclim = builder.elmocclim;
    this.elmpocclim1 = builder.elmpocclim1;
    this.elmpocclim2 = builder.elmpocclim2;
    this.elmclslim = builder.elmclslim;
    this.gluered = builder.gluered;
    this.gluekeep = builder.gluekeep;
    this.glueupdate = builder.glueupdate;
    this.itsimpdel = builder.itsimpdel;
    this.plain = builder.plain;
    this.restart = builder.restart;
    this.restartint = builder.restartint;
    this.redinit = builder.redinit;
    this.redinc = builder.redinc;
    this.reusetrail = builder.reusetrail;
    this.simpint = builder.simpint;
    this.simpgeom = builder.simpgeom;
    this.sizepen = builder.sizepen;
    this.sizemaxpen = builder.sizemaxpen;
    this.searchint = builder.searchint;
    this.searchfirst = builder.searchfirst;
    this.scincfact = builder.scincfact;
    this.stepslim = builder.stepslim;
  }

  /**
   * Returns whether the solver is in 'plain' mode or not.
   * @return {@code true} if the solver is in plain mode, {@code false} otherwise
   */
  public boolean plain() {
    return this.plain;
  }
}
