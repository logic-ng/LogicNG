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
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Substitution;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for the class {@link Substitution}.
 * @version 1.0
 * @since 1.0
 */
public class SubstitutionTest {

  private Substitution subst;

  public SubstitutionTest() {
    subst = new Substitution();
    subst.addMapping(F.A, F.NA);
    subst.addMapping(F.B, F.OR1);
    subst.addMapping(F.X, F.AND1);
  }

  @Test
  public void testConstructor() {
    Assert.assertNotNull(new Substitution());
  }

  @Test
  public void testSize() {
    final Substitution subst = new Substitution();
    subst.addMapping(F.A, F.NA);
    subst.addMapping(F.B, F.OR1);
    subst.addMapping(F.C, F.AND1);
    Assert.assertEquals(3, subst.size());
  }

  @Test
  public void testGetSubstitution() {
    final Substitution subst = new Substitution();
    subst.addMapping(F.A, F.NA);
    subst.addMapping(F.B, F.OR1);
    subst.addMapping(F.C, F.AND1);
    Assert.assertEquals(F.NA, subst.getSubstitution(F.A));
    Assert.assertEquals(F.OR1, subst.getSubstitution(F.B));
    Assert.assertEquals(F.AND1, subst.getSubstitution(F.C));
    Assert.assertNull(subst.getSubstitution(F.X));
    subst.addMapping(F.B, F.AND1);
    Assert.assertEquals(F.AND1, subst.getSubstitution(F.B));
  }

  @Test
  public void testConstantSubstitution() {
    Assert.assertEquals(F.FALSE, F.FALSE.substitute(subst));
    Assert.assertEquals(F.TRUE, F.TRUE.substitute(subst));
  }

  @Test
  public void testLiteralSubstitution() {
    Assert.assertEquals(F.C, F.C.substitute(subst));
    Assert.assertEquals(F.NA, F.A.substitute(subst));
    Assert.assertEquals(F.OR1, F.B.substitute(subst));
    Assert.assertEquals(F.AND1, F.X.substitute(subst));
    Assert.assertEquals(F.A, F.NA.substitute(subst));
    Assert.assertEquals(F.NOT2, F.NB.substitute(subst));
    Assert.assertEquals(F.NOT1, F.NX.substitute(subst));
  }

  @Test
  public void testNotSubstitution() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~(~a & (x | y))"), F.NOT1.substitute(subst));
    Assert.assertEquals(p.parse("~(a & b | y)"), F.NOT2.substitute(subst));
  }

  @Test
  public void testBinarySubstitution() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~a => (x | y)"), F.IMP1.substitute(subst));
    Assert.assertEquals(p.parse("(~a <=> (x | y)) => (~(a & b) <=> ~y)"), F.IMP4.substitute(subst));
    Assert.assertEquals(p.parse("a <=> ~(x | y)"), F.EQ2.substitute(subst));
    Assert.assertEquals(p.parse("(~a & (x | y)) <=> (a & b | y)"), F.EQ3.substitute(subst));
  }

  @Test
  public void testNArySubstitution() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("(a & b | y) & (~(a & b) | ~y)"), F.AND3.substitute(subst));
    Assert.assertEquals(p.parse("~(x | y) & c & a & b & ~y"), F.f.and(F.NB, F.C, F.X, F.NY).substitute(subst));
    Assert.assertEquals(p.parse("(~a & (x | y)) | (a & ~(x | y))"), F.OR3.substitute(subst));
    Assert.assertEquals(p.parse("~a | ~(x | y) | c | a & b | ~y"), F.f.or(F.A, F.NB, F.C, F.X, F.NY).substitute(subst));
  }

  @Test
  public void testSingleSubstitution() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("x | y"), F.A.substitute(F.A, F.OR1));
    Assert.assertEquals(p.parse("~(x | y)"), F.NA.substitute(F.A, F.OR1));
    Assert.assertEquals(p.parse("a => (x | y)"), F.IMP1.substitute(F.B, F.OR1));
    Assert.assertEquals(p.parse("~a <=> ~(x | y)"), F.EQ2.substitute(F.B, F.OR1));
    Assert.assertEquals(p.parse("a & ~b & c & ~x"), F.f.and(F.A, F.NB, F.C, F.NX, F.NY).substitute(F.Y, F.X));
    Assert.assertEquals(p.parse("a | ~b | c | ~x"), F.f.or(F.A, F.NB, F.C, F.NX, F.NY).substitute(F.Y, F.X));
  }

  @Test
  public void testHashCode() {
    final Substitution subst = new Substitution();
    subst.addMapping(F.A, F.NA);
    subst.addMapping(F.B, F.OR1);
    subst.addMapping(F.C, F.AND1);
    final Substitution subst2 = new Substitution();
    subst2.addMapping(F.B, F.OR1);
    subst2.addMapping(F.C, F.AND1);
    subst2.addMapping(F.A, F.NA);
    Assert.assertEquals(subst.hashCode(), subst2.hashCode());
  }

  @Test
  public void testEquals() {
    final Substitution subst = new Substitution();
    subst.addMapping(F.A, F.NA);
    subst.addMapping(F.B, F.OR1);
    subst.addMapping(F.C, F.AND1);
    final Substitution subst2 = new Substitution();
    subst2.addMapping(F.B, F.OR1);
    subst2.addMapping(F.C, F.AND1);
    subst2.addMapping(F.A, F.NA);
    final Substitution subst3 = new Substitution();
    subst3.addMapping(F.B, F.OR1);
    subst3.addMapping(F.C, F.AND1);
    Assert.assertEquals(subst, subst2);
    Assert.assertEquals(subst, subst);
    Assert.assertNotEquals(subst, null);
    Assert.assertNotEquals(subst, new Assignment());
    Assert.assertNotEquals(subst, subst3);
  }

  @Test
  public void testToString() {
    final Substitution subst = new Substitution();
    Assert.assertEquals("Substitution{}", subst.toString());
    subst.addMapping(F.A, F.NA);
    Assert.assertEquals("Substitution{a=~a}", subst.toString());
    subst.addMapping(F.B, F.OR1);
  }

}
