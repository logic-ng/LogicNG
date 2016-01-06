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

import org.junit.Assert;
import org.junit.Test;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;

/**
 * Unit tests for the solver configurations.
 * @author Christoph Zengler
 * @version 1.0
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
            .build();
    final String expected = "MiniSatConfig{\n" +
            "varDecay=1.2\n" +
            "varInc=1.3\n" +
            "clauseMin=BASIC\n" +
            "restartFirst=200\n" +
            "restartInc=0.8\n" +
            "clauseDecay=0.92\n" +
            "removeSatisfied=true\n" +
            "learntsizeFactor=1.4\n" +
            "learntsizeInc=1.5\n" +
            "incremental=false\n" +
            "}\n";
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
    final String expected = "GlucoseConfig{\n" +
            "lbLBDMinimizingClause=3\n" +
            "lbLBDFrozenClause=25\n" +
            "lbSizeMinimizingClause=24\n" +
            "firstReduceDB=1999\n" +
            "specialIncReduceDB=999\n" +
            "incReduceDB=299\n" +
            "factorK=0.7\n" +
            "factorR=1.3\n" +
            "sizeLBDQueue=45\n" +
            "sizeTrailQueue=4999\n" +
            "reduceOnSize=true\n" +
            "reduceOnSizeSize=10\n" +
            "maxVarDecay=0.99\n" +
            "}\n";
    Assert.assertEquals(expected, config.toString());
  }

  @Test
  public void testCleaneLingConfigToString() {
    final CleaneLingConfig config = new CleaneLingConfig.Builder()
            .bce(false)
            .blkwait(2)
            .blkrtc(1)
            .boost(11)
            .bwclslim(9999)
            .bwocclim(9998)
            .cbump(CleaneLingConfig.ClauseBumping.AVG)
            .distill(false)
            .elim(false)
            .elmrtc(3)
            .elmocclim(9997)
            .elmpocclim1(99)
            .elmpocclim2(7)
            .elmclslim(999)
            .gluered(false)
            .gluekeep(4)
            .glueupdate(true)
            .itsimpdel(9)
            .plain(true)
            .restart(false)
            .restartint(98)
            .redinit(998)
            .redinc(1999)
            .reusetrail(false)
            .simpint(99999)
            .simpgeom(false)
            .sizepen(-1)
            .sizemaxpen(6)
            .searchint(4999)
            .searchfirst(true)
            .scincfact(1049)
            .stepslim(999999)
            .build();
    final String expected = "CleaneLingConfig{\n" +
            "block=false\n" +
            "blkwait=2\n" +
            "blkrtc=1\n" +
            "boost=11\n" +
            "bwclslim=9999\n" +
            "bwocclim=9998\n" +
            "cbump=AVG\n" +
            "distill=false\n" +
            "elim=false\n" +
            "elmrtc=3\n" +
            "elmocclim=9997\n" +
            "elmpocclim1=99\n" +
            "elmpocclim2=7\n" +
            "elmclslim=999\n" +
            "gluered=false\n" +
            "gluekeep=4\n" +
            "glueupdate=true\n" +
            "itsimpdel=9\n" +
            "plain=true\n" +
            "restart=false\n" +
            "restartint=98\n" +
            "redinit=998\n" +
            "redinc=1999\n" +
            "reusetrail=false\n" +
            "simpint=99999\n" +
            "simpgeom=false\n" +
            "sizepen=-1\n" +
            "sizemaxpen=6\n" +
            "searchint=4999\n" +
            "searchfirst=true\n" +
            "scincfact=1049\n" +
            "stepslim=999999\n" +
            "}\n";
    Assert.assertEquals(expected, config.toString());
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
    final String expected = "MaxSATConfig{incrementalStrategy=ITERATIVE\n" +
            "amoEncoding=LADDER\n" +
            "pbEncoding=SWC\n" +
            "cardinalityEncoding=MTOTALIZER\n" +
            "weightStrategy=DIVERSIFY\n" +
            "solverType=MINISAT\n" +
            "verbosity=SOME\n" +
            "symmetry=false\n" +
            "limit=1000\n" +
            "bmo=false\n" +
            "}\n";
    Assert.assertEquals(expected, config.toString());
  }

}
