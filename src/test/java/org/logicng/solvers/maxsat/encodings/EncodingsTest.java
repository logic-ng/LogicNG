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

package org.logicng.solvers.maxsat.encodings;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;

/**
 * Unit test for the package {@link org.logicng.solvers.maxsat.encodings}.
 * @version 1.1
 * @since 1.1
 */
public class EncodingsTest {

  @Test
  public void testEncoder() {
    Encoder encoder = new Encoder(MaxSATConfig.CardinalityEncoding.TOTALIZER);
    Assert.assertEquals("Encoder", encoder.toString());
  }

  @Test
  public void testTotalizer() {
    Totalizer totalizer = new Totalizer(MaxSATConfig.IncrementalStrategy.ITERATIVE);
    Assert.assertEquals(MaxSATConfig.IncrementalStrategy.ITERATIVE, totalizer.incremental());
    Assert.assertEquals("Totalizer", totalizer.toString());
  }

  @Test
  public void testModularTotalizer() {
    ModularTotalizer mTotalizer = new ModularTotalizer();
    Assert.assertEquals(false, mTotalizer.hasCreatedEncoding());
    Assert.assertEquals("ModularTotalizer", mTotalizer.toString());
  }

  @Test
  public void testSequentialWeightCounter() {
    SequentialWeightCounter swc = new SequentialWeightCounter();
    Assert.assertEquals(false, swc.hasCreatedEncoding());
    Assert.assertEquals("SequentialWeightCounter", swc.toString());
  }

  @Test
  public void testLadder() {
    Ladder ladder = new Ladder();
    Assert.assertEquals("Ladder", ladder.toString());
  }
}
