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
 * Unit Tests for the class {@link CTrue}.
 * @version 1.0
 * @since 1.0
 */
public class CTrueTest {

  @Test
  public void testType() {
    Assert.assertEquals(FType.TRUE, F.TRUE.type());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(1, F.TRUE.numberOfAtoms());
    Assert.assertEquals(1, F.TRUE.numberOfAtoms());
  }

  @Test
  public void testNegation() {
    Assert.assertEquals(F.FALSE, F.TRUE.negate());
  }

  @Test
  public void testVariables() {
    Assert.assertEquals(0, F.TRUE.variables().size());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(0, F.TRUE.literals().size());
  }

  @Test
  public void testToString() {
    Assert.assertEquals("$true", F.TRUE.toString());
  }

  @Test
  public void testEquals() {
    Assert.assertEquals(F.TRUE, F.f.verum());
    Assert.assertNotEquals(F.TRUE, null);
    Assert.assertNotEquals(F.TRUE, F.f.falsum());
  }

  @Test
  public void testEqualsDifferentFormulaFactory() {
    Assert.assertEquals(F.TRUE, F.g.verum());
    Assert.assertNotEquals(F.TRUE, F.g.falsum());
  }

  @Test
  public void testHash() {
    Assert.assertEquals(F.f.verum().hashCode(), F.TRUE.hashCode());
  }

  @Test
  public void testNumberOfNodes() {
    Assert.assertEquals(1, F.TRUE.numberOfNodes());
  }

  @Test
  public void testNumberOfInternalNodes() {
    Assert.assertEquals(1, F.TRUE.numberOfInternalNodes());
  }

  @Test
  public void testNumberOfOperands() {
    Assert.assertEquals(0, F.TRUE.numberOfOperands());
  }

  @Test
  public void testIsConstantFormula() {
    Assert.assertTrue(F.TRUE.isConstantFormula());
  }

  @Test
  public void testAtomicFormula() {
    Assert.assertTrue(F.TRUE.isAtomicFormula());
  }

  @Test
  public void testContains() {
    Assert.assertFalse(F.TRUE.containsVariable(F.f.variable("a")));
  }
}
