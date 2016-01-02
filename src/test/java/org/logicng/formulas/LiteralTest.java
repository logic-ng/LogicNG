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

package org.logicng.formulas;

import org.logicng.datastructures.Substitution;
import org.junit.Assert;
import org.junit.Test;
import org.logicng.io.parsers.ParserException;

/**
 * Unit Tests for the class {@link Literal}.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public class LiteralTest {

  @Test
  public void testType() {
    Assert.assertEquals(FType.LITERAL, F.A.type());
    Assert.assertEquals(FType.LITERAL, F.NA.type());
  }

  @Test
  public void testShortcutCreators() {
    Assert.assertEquals(F.f.literal("a"), F.f.literal("a", true));
    Assert.assertEquals(F.f.literal("name"), F.f.literal("name", true));
  }

  @Test
  public void testNegation() {
    Assert.assertEquals(F.NA, F.A.negate());
    Assert.assertEquals(F.A, F.NA.negate());
  }

  @Test
  public void testGetters() {
    Assert.assertEquals("a", F.A.name());
    Assert.assertEquals("a", F.NA.name());
    Assert.assertEquals(true, F.A.phase());
    Assert.assertEquals(false, F.NA.phase());
  }

  @Test
  public void testVariables() {
    Assert.assertEquals(1, F.A.variables().size());
    Assert.assertEquals(F.A, F.A.variables().first());
    Assert.assertEquals(1, F.NA.variables().size());
    Assert.assertEquals(F.A, F.NA.variables().first());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(1, F.A.literals().size());
    Assert.assertEquals(F.A, F.A.literals().first());
    Assert.assertEquals(1, F.NA.literals().size());
    Assert.assertEquals(F.NA, F.NA.literals().first());
  }

  @Test
  public void testExpSubstitution() {
    final Substitution substitution = new Substitution();
    substitution.addMapping(F.f.literal("a"), F.f.literal("b", false));
    substitution.addMapping(F.f.literal("c"), F.f.literal("d"));
    substitution.addMapping(F.f.literal("x"), F.f.and(F.f.literal("y"), F.f.literal("z")));
  }

  @Test
  public void testToString() {
    Assert.assertEquals("a", F.A.toString());
    Assert.assertEquals("~a", F.NA.toString());
  }

  @Test
  public void testEquals() {
    Assert.assertEquals(F.A, F.f.literal("a", true));
    Assert.assertEquals(F.NA, F.f.literal("a", false));
    Assert.assertEquals(F.A, F.A);
    Assert.assertNotEquals(F.A, F.B);
    Assert.assertNotEquals(F.A, F.NA);
    Assert.assertNotEquals(F.A, null);
    Assert.assertNotEquals(F.A, F.f.falsum());
  }

  @Test
  public void testEqualsDifferentFormulaFactory() {
    Assert.assertEquals(F.A, F.g.literal("a", true));
    Assert.assertEquals(F.NA, F.g.literal("a", false));
    Assert.assertNotEquals(F.A, F.g.literal("a", false));
    Assert.assertNotEquals(F.A, F.g.literal("b", true));
    Assert.assertNotEquals(F.A, F.g.falsum());
  }

  @Test
  public void testCompareTo() {
    Assert.assertTrue(F.A.compareTo(F.A) == 0);
    Assert.assertTrue(F.NA.compareTo(F.NA) == 0);
    Assert.assertTrue(F.A.compareTo(F.NA) < 0);
    Assert.assertTrue(F.A.compareTo(F.NB) < 0);
    Assert.assertTrue(F.A.compareTo(F.B) < 0);
    Assert.assertTrue(F.A.compareTo(F.X) < 0);
    Assert.assertTrue(F.NA.compareTo(F.NX) < 0);
  }

  @Test
  public void testHash() {
    Assert.assertEquals(F.A.hashCode(), F.f.literal("a", true).hashCode());
    Assert.assertEquals(F.NA.hashCode(), F.f.literal("a", false).hashCode());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(1, F.A.numberOfAtoms());
    Assert.assertEquals(1, F.NA.numberOfAtoms());
    Assert.assertEquals(1, F.NA.numberOfAtoms());
  }

  @Test
  public void testNumberOfNodes() {
    Assert.assertEquals(1, F.A.numberOfNodes());
    Assert.assertEquals(1, F.NA.numberOfNodes());
    Assert.assertEquals(1, F.NA.numberOfNodes());
  }

  @Test
  public void testNumberOfInternalNodes() throws ParserException {
    Assert.assertEquals(1, F.A.numberOfInternalNodes());
    Assert.assertEquals(1, F.NA.numberOfInternalNodes());
  }

  @Test
  public void testAtomicFormula() {
    Assert.assertTrue(F.A.isAtomicFormula());
    Assert.assertTrue(F.NA.isAtomicFormula());
  }

  @Test
  public void testContains() {
    Assert.assertFalse(F.A.contains(F.f.literal("b")));
    Assert.assertTrue(F.A.contains(F.f.literal("a")));
    Assert.assertFalse(F.NA.contains(F.f.literal("b")));
    Assert.assertTrue(F.NA.contains(F.f.literal("a")));
  }
}
