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

import org.junit.Assert;
import org.junit.Test;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;

import java.util.Arrays;

/**
 * Unit tests for the solver configurations.
 * @version 1.3
 * @since 1.0
 */
public class ConfigurationsTest {

  @Test
  public void testMiniSatConfigToString() {
    final MiniSatConfig config = new MiniSatConfig.Builder()
            .varDecay(1.2)
            .varInc(1.3)
            .clMinimization(MiniSatConfig.ClauseMinimization.BASIC)
            .restartFirst(200)
            .restartInc(0.8)
            .clauseDecay(0.92)
            .removeSatisfied(true)
            .lsFactor(1.4)
            .lsInc(1.5)
            .incremental(false)
            .initialPhase(true)
            .build();
    final String expected = String.format("MiniSatConfig{%n" +
            "varDecay=1.2%n" +
            "varInc=1.3%n" +
            "clauseMin=BASIC%n" +
            "restartFirst=200%n" +
            "restartInc=0.8%n" +
            "clauseDecay=0.92%n" +
            "removeSatisfied=true%n" +
            "learntsizeFactor=1.4%n" +
            "learntsizeInc=1.5%n" +
            "incremental=false%n" +
            "initialPhase=true%n" +
            "proofGeneration=false%n" +
            "}%n");
    Assert.assertEquals(expected, config.toString());
  }

  @Test
  public void testGlucoseConfigToString() {
    final GlucoseConfig config = new GlucoseConfig.Builder()
            .lbLBDMinimizingClause(3)
            .lbLBDFrozenClause(25)
            .lbSizeMinimizingClause(24)
            .firstReduceDB(1999)
            .specialIncReduceDB(999)
            .incReduceDB(299)
            .factorK(0.7)
            .factorR(1.3)
            .sizeLBDQueue(45)
            .sizeTrailQueue(4999)
            .reduceOnSize(true)
            .reduceOnSizeSize(10)
            .maxVarDecay(0.99)
            .build();
    final String expected = String.format("GlucoseConfig{%n" +
            "lbLBDMinimizingClause=3%n" +
            "lbLBDFrozenClause=25%n" +
            "lbSizeMinimizingClause=24%n" +
            "firstReduceDB=1999%n" +
            "specialIncReduceDB=999%n" +
            "incReduceDB=299%n" +
            "factorK=0.7%n" +
            "factorR=1.3%n" +
            "sizeLBDQueue=45%n" +
            "sizeTrailQueue=4999%n" +
            "reduceOnSize=true%n" +
            "reduceOnSizeSize=10%n" +
            "maxVarDecay=0.99%n" +
            "}%n");
    Assert.assertEquals(expected, config.toString());
  }

  @Test
  public void testCleaneLingConfigToString() {
    final CleaneLingConfig config = new CleaneLingConfig.Builder()
            .blockedClauseElimination(false)
            .blockedClauseEliminationWait(2)
            .blockedClauseEliminationRTC(1)
            .boost(11)
            .bwClauseLim(9999)
            .bwOccurrenceLim(9998)
            .clauseBumping(CleaneLingConfig.ClauseBumping.AVG)
            .distillation(false)
            .bvElim(false)
            .bvElimRTC(3)
            .bvElimOccurrenceLim(9997)
            .bvElimPivotOccurrenceLimOneSided(99)
            .bvElimPivotOccurrenceLimTwoSided(7)
            .bvElimClauseLim(999)
            .gluered(false)
            .glueKeep(4)
            .glueUpdate(true)
            .iterationSimplificationDelay(9)
            .plain(true)
            .restart(false)
            .restartInterval(98)
            .reductionInterval(998)
            .reductionIntervalInc(1999)
            .reuseTrail(false)
            .simpSteps(99999)
            .simpGeomIncrease(false)
            .sizePenalty(-1)
            .sizeMaxPenalty(6)
            .searchInterval(4999)
            .searchFirst(true)
            .scoreIncrementFactor(1049)
            .stepsLim(999999)
            .build();
    final String expected = String.format("CleaneLingConfig{%n" +
            "blockedClauseElimination=false%n" +
            "blockedClauseEliminationWait=2%n" +
            "blockedClauseEliminationRTC=1%n" +
            "boost=11%n" +
            "bwClauseLim=9999%n" +
            "bwOccurrenceLim=9998%n" +
            "clauseBumping=AVG%n" +
            "distillation=false%n" +
            "bvElim=false%n" +
            "bvElimRTC=3%n" +
            "bvElimOccurrenceLim=9997%n" +
            "bvElimPivotOccurrenceLimOneSided=99%n" +
            "bvElimPivotOccurrenceLimTwoSided=7%n" +
            "bvElimClauseLim=999%n" +
            "gluered=false%n" +
            "glueKeep=4%n" +
            "glueUpdate=true%n" +
            "iterationSimplificationDelay=9%n" +
            "plain=true%n" +
            "restart=false%n" +
            "restartInterval=98%n" +
            "reductionInterval=998%n" +
            "reductionIntervalInc=1999%n" +
            "reuseTrail=false%n" +
            "simpSteps=99999%n" +
            "simpGeomIncrease=false%n" +
            "sizePenalty=-1%n" +
            "sizeMaxPenalty=6%n" +
            "searchInterval=4999%n" +
            "searchFirst=true%n" +
            "scoreIncrementFactor=1049%n" +
            "stepsLim=999999%n" +
            "}%n");
    Assert.assertEquals(expected, config.toString());
  }

  @Test
  public void testClauseBumping() {
    Assert.assertTrue(Arrays.asList(CleaneLingConfig.ClauseBumping.values()).contains(CleaneLingConfig.ClauseBumping.valueOf("AVG")));
  }

  @Test
  public void testMaxSATConfigToString() {
    final MaxSATConfig config = new MaxSATConfig.Builder()
            .incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE)
            .cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER)
            .weight(MaxSATConfig.WeightStrategy.DIVERSIFY)
            .solver(MaxSATConfig.SolverType.MINISAT)
            .verbosity(MaxSATConfig.Verbosity.SOME)
            .output(System.out)
            .symmetry(false)
            .limit(1000)
            .bmo(false)
            .build();
    final String expected = String.format("MaxSATConfig{%n" +
            "incrementalStrategy=ITERATIVE%n" +
            "pbEncoding=LADDER%n" +
            "pbEncoding=SWC%n" +
            "cardinalityEncoding=MTOTALIZER%n" +
            "weightStrategy=DIVERSIFY%n" +
            "solverType=MINISAT%n" +
            "verbosity=SOME%n" +
            "symmetry=false%n" +
            "limit=1000%n" +
            "bmo=false%n" +
            "}%n");
    Assert.assertEquals(expected, config.toString());
  }

}
