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

package org.logicng.predicates;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.F;

/**
 * Unit tests for the cnf predicate.
 * @version 1.1
 * @since 1.0
 */
public class CNFPredicateTest {

  private final CNFPredicate cnfPredicate = new CNFPredicate();

  @Test
  public void test() {
    Assert.assertTrue(F.f.verum().holds(cnfPredicate));
    Assert.assertTrue(F.f.falsum().holds(cnfPredicate));
    Assert.assertTrue(F.A.holds(cnfPredicate));
    Assert.assertTrue(F.NA.holds(cnfPredicate));
    Assert.assertTrue(F.OR1.holds(cnfPredicate));
    Assert.assertTrue(F.AND1.holds(cnfPredicate));
    Assert.assertTrue(F.AND3.holds(cnfPredicate));
    Assert.assertTrue(F.f.and(F.OR1, F.OR2, F.A, F.NY).holds(cnfPredicate));
    Assert.assertFalse(F.PBC1.holds(cnfPredicate));
    Assert.assertFalse(F.OR3.holds(cnfPredicate));
    Assert.assertFalse(F.IMP1.holds(cnfPredicate));
    Assert.assertFalse(F.EQ1.holds(cnfPredicate));
    Assert.assertFalse(F.NOT1.holds(cnfPredicate));
    Assert.assertFalse(F.NOT2.holds(cnfPredicate));
    Assert.assertFalse(F.f.and(F.OR1, F.EQ1).holds(cnfPredicate));
  }

  @Test
  public void testToString() {
    Assert.assertEquals("CNFPredicate", cnfPredicate.toString());
  }
}
