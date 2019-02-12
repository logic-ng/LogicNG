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

package org.logicng.formulas;

import org.junit.Assert;
import org.junit.Test;

/**
 * Unit Tests for the class {@link CFalse}.
 * @version 1.0
 * @since 1.0
 */
public class CFalseTest {

  @Test
  public void testType() {
    Assert.assertEquals(FType.FALSE, F.FALSE.type());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(1, F.FALSE.numberOfAtoms());
  }

  @Test
  public void testNegation() {
    Assert.assertEquals(F.TRUE, F.FALSE.negate());
  }

  @Test
  public void testVariables() {
    Assert.assertEquals(0, F.FALSE.variables().size());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(0, F.FALSE.literals().size());
  }

  @Test
  public void testToString() {
    Assert.assertEquals("$false", F.FALSE.toString());
  }

  @Test
  public void testEquals() {
    Assert.assertEquals(F.FALSE, F.f.falsum());
    Assert.assertNotEquals(F.FALSE, null);
    Assert.assertNotEquals(F.FALSE, F.f.verum());
  }

  @Test
  public void testEqualsDifferentFormulaFactory() {
    Assert.assertEquals(F.FALSE, F.g.falsum());
    Assert.assertNotEquals(F.FALSE, F.g.verum());
  }

  @Test
  public void testHash() {
    Assert.assertEquals(F.f.falsum().hashCode(), F.FALSE.hashCode());
  }

  @Test
  public void testNumberOfNodes() {
    Assert.assertEquals(1, F.FALSE.numberOfNodes());
  }

  @Test
  public void testNumberOfInternalNodes() {
    Assert.assertEquals(1, F.FALSE.numberOfInternalNodes());
  }

  @Test
  public void testNumberOfOperands() {
    Assert.assertEquals(0, F.FALSE.numberOfOperands());
  }


  @Test
  public void testIsConstantFormula() {
    Assert.assertTrue(F.FALSE.isConstantFormula());
  }

  @Test
  public void testAtomicFormula() {
    Assert.assertTrue(F.FALSE.isAtomicFormula());
  }

  @Test
  public void testContains() {
    Assert.assertFalse(F.FALSE.containsVariable(F.f.variable("a")));
  }
}
