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

package org.logicng.datastructures;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Unit tests for the class {@link Assignment}.
 * @version 1.1
 * @since 1.0
 */
public class AssignmentTest {

  @Test
  public void testCreators() {
    Assert.assertNotNull(new Assignment(Arrays.asList(F.A, F.B, F.X, F.Y)));
  }

  @Test
  public void testSize() {
    Assert.assertEquals(4, new Assignment(Arrays.asList(F.A, F.B, F.X, F.Y), true).size());
    Assert.assertEquals(4, new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), false).size());
    Assert.assertEquals(2, new Assignment(Arrays.asList(F.A, F.NB)).size());
  }

  @Test
  public void testPositiveLiterals() {
    Literal[] a = {F.A, F.B, F.X, F.Y};
    Assignment ass1 = new Assignment(Arrays.asList(a), false);
    Assert.assertEquals(Arrays.asList(a), ass1.positiveLiterals());
    ass1 = new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY));
    Assert.assertEquals(Arrays.asList(F.A, F.B), ass1.positiveLiterals());
    ass1 = new Assignment(Arrays.asList(F.NA, F.NB, F.NX, F.NY));
    Assert.assertEquals(0, ass1.positiveLiterals().size());
  }

  @Test
  public void testNegativeLiterals() {
    Literal[] a = {F.NA, F.NB, F.NX, F.NY};
    Assignment ass = new Assignment(Arrays.asList(a));
    Assert.assertEquals(Arrays.asList(a), ass.negativeLiterals());
    ass = new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY));
    Assert.assertEquals(Arrays.asList(F.NX, F.NY), ass.negativeLiterals());
    ass = new Assignment(Arrays.asList(F.A, F.B, F.X, F.Y));
    Assert.assertEquals(0, ass.negativeLiterals().size());
  }

  @Test
  public void testNegativeVariables() {
    Literal[] a = {F.A, F.B, F.X, F.Y};
    Literal[] na = {F.NA, F.NB, F.NX, F.NY};
    Assignment ass = new Assignment(Arrays.asList(na));
    Assert.assertEquals(Arrays.asList(a), ass.negativeVariables());
    ass = new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY));
    Assert.assertEquals(Arrays.asList(F.X, F.Y), ass.negativeVariables());
    ass = new Assignment(Arrays.asList(F.A, F.B, F.X, F.Y));
    Assert.assertEquals(0, ass.negativeVariables().size());
  }

  @Test
  public void testAddLiteral() {
    Assignment ass = new Assignment();
    ass.addLiteral(F.A);
    ass.addLiteral(F.B);
    ass.addLiteral(F.NX);
    ass.addLiteral(F.NY);
    Assert.assertEquals(Arrays.asList(F.A, F.B), ass.positiveLiterals());
    Assert.assertEquals(Arrays.asList(F.NX, F.NY), ass.negativeLiterals());
  }

  @Test
  public void testEvaluateLit() {
    Assignment ass = new Assignment(Arrays.asList(F.A, F.NX));
    Assert.assertTrue(ass.evaluateLit(F.A));
    Assert.assertTrue(ass.evaluateLit(F.NX));
    Assert.assertTrue(ass.evaluateLit(F.NB));
    Assert.assertFalse(ass.evaluateLit(F.NA));
    Assert.assertFalse(ass.evaluateLit(F.X));
    Assert.assertFalse(ass.evaluateLit(F.B));
  }

  @Test
  public void testRestrictLit() {
    Assignment ass = new Assignment(Arrays.asList(F.A, F.NX));
    Assert.assertEquals(F.TRUE, ass.restrictLit(F.A));
    Assert.assertEquals(F.TRUE, ass.restrictLit(F.NX));
    Assert.assertEquals(F.FALSE, ass.restrictLit(F.NA));
    Assert.assertEquals(F.FALSE, ass.restrictLit(F.X));
    Assert.assertEquals(F.B, ass.restrictLit(F.B));
    Assert.assertEquals(F.NB, ass.restrictLit(F.NB));
  }

  @Test
  public void testFormula() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("a"), new Assignment(Collections.singletonList(F.A)).formula(F.f));
    Assert.assertEquals(p.parse("~a"), new Assignment(Collections.singletonList(F.NA)).formula(F.f));
    Assert.assertEquals(p.parse("a & b"), new Assignment(Arrays.asList(F.A, F.B)).formula(F.f));
    Assert.assertEquals(p.parse("a & b & ~x & ~y"), new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)).formula(F.f));
  }

  @Test
  public void testFastEvaluable() {
    Assignment ass = new Assignment(Arrays.asList(F.A, F.NX), false);
    Assert.assertFalse(ass.fastEvaluable());
    ass.convertToFastEvaluable();
    Assert.assertTrue(ass.fastEvaluable());
    Assert.assertEquals(Collections.singletonList(F.A), ass.positiveLiterals());
    Assert.assertEquals(Collections.singletonList(F.NX), ass.negativeLiterals());
    Assert.assertEquals(Collections.singletonList(F.X), ass.negativeVariables());
    ass.addLiteral(F.NB);
    ass.addLiteral(F.Y);
    Assert.assertEquals(Arrays.asList(F.A, F.Y), ass.positiveLiterals());
    Assert.assertEquals(Arrays.asList(F.NB, F.NX), ass.negativeLiterals());
    Assert.assertEquals(Arrays.asList(F.X, F.B), ass.negativeVariables());
    Assert.assertTrue(ass.evaluateLit(F.Y));
    Assert.assertFalse(ass.evaluateLit(F.B));
    Assert.assertEquals(F.TRUE, ass.restrictLit(F.NB));
    Assert.assertEquals(F.FALSE, ass.restrictLit(F.X));
    Assert.assertEquals(F.C, ass.restrictLit(F.C));
    Assert.assertEquals(F.f.and(F.A, F.NX, F.NB, F.Y), ass.formula(F.f));
    ass = new Assignment(Arrays.asList(F.A, F.NX), true);
    Assert.assertTrue(ass.fastEvaluable());
    ass.convertToFastEvaluable();
    Assert.assertTrue(ass.fastEvaluable());
  }

  @Test
  public void testHashCode() {
    Assignment ass = new Assignment();
    ass.addLiteral(F.A);
    ass.addLiteral(F.B);
    ass.addLiteral(F.NX);
    ass.addLiteral(F.NY);
    Assert.assertEquals(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)).hashCode(), new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)).hashCode());
    Assert.assertEquals(ass.hashCode(), new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)).hashCode());
    Assert.assertEquals(ass.hashCode(), new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)).hashCode());
  }

  @Test
  public void testEquals() {
    Assignment ass = new Assignment();
    ass.addLiteral(F.A);
    ass.addLiteral(F.B);
    ass.addLiteral(F.NX);
    ass.addLiteral(F.NY);
    Assert.assertEquals(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), false), new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), false));
    Assert.assertEquals(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), false), new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), true));
    Assert.assertEquals(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), true), new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), false));
    Assert.assertEquals(new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), true), new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY), true));
    Assert.assertEquals(ass, new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)));
    Assert.assertEquals(ass, new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY)));
    Assert.assertEquals(ass, ass);
    Assert.assertNotEquals(ass, new Assignment(Arrays.asList(F.A, F.B, F.NX)));
    Assert.assertNotEquals(ass, new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY, F.C)));
    Assert.assertNotEquals(ass, null);
    Assert.assertNotEquals(ass, F.TRUE);
  }

  @Test
  public void testBlockingClause() {
    Assignment ass = new Assignment();
    ass.addLiteral(F.A);
    ass.addLiteral(F.B);
    ass.addLiteral(F.NX);
    ass.addLiteral(F.NY);
    Formula bc01 = ass.blockingClause(F.f);
    Assert.assertFalse(bc01.containsVariable(F.C));
    Assert.assertEquals("~a | ~b | x | y", bc01.toString());
    Formula bc02 = ass.blockingClause(F.f, null);
    Assert.assertFalse(bc02.containsVariable(F.C));
    Assert.assertEquals("~a | ~b | x | y", bc02.toString());
    List<Literal> lits = new ArrayList<>();
    lits.add(F.A);
    lits.add(F.X);
    lits.add(F.C);
    Formula bcProjected = ass.blockingClause(F.f, lits);
    Assert.assertFalse(bcProjected.containsVariable(F.C));
    Assert.assertEquals("~a | x", bcProjected.toString());
  }

  @Test
  public void testToString() {
    Assert.assertEquals("Assignment{pos=[], neg=[]}", new Assignment().toString());
    Assert.assertEquals("Assignment{pos=[a], neg=[]}", new Assignment(Collections.singletonList(F.A)).toString());
    Assert.assertEquals("Assignment{pos=[], neg=[~a]}", new Assignment(Collections.singletonList(F.NA)).toString());
    Assert.assertEquals("Assignment{pos=[a, b, c], neg=[~x, ~y]}", new Assignment(Arrays.asList(F.A, F.B, F.NX, F.NY, F.C)).toString());
  }
}
