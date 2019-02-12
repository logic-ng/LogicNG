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
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link Implication}.
 * @version 1.0
 * @since 1.0
 */
public class ImplicationTest {

  @Test
  public void testType() {
    Assert.assertEquals(FType.IMPL, F.IMP1.type());
  }

  @Test
  public void testCreator() {
    Assert.assertEquals(F.TRUE, F.f.implication(F.FALSE, F.A));
    Assert.assertEquals(F.TRUE, F.f.implication(F.A, F.TRUE));
    Assert.assertEquals(F.A, F.f.implication(F.TRUE, F.A));
    Assert.assertEquals(F.NA, F.f.implication(F.A, F.FALSE));
    Assert.assertEquals(F.TRUE, F.f.implication(F.A, F.A));
    Assert.assertEquals(F.IMP3, F.f.binaryOperator(FType.IMPL, F.AND1, F.OR1));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalCreation() {
    F.f.binaryOperator(FType.NOT, F.AND1, F.OR1);
  }

  @Test
  public void testGetters() {
    Assert.assertEquals(F.NA, ((Implication) F.IMP2).left());
    Assert.assertEquals(F.NB, ((Implication) F.IMP2).right());
  }

  @Test
  public void testVariables() {
    Assert.assertEquals(4, F.IMP3.variables().size());
    SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
    Assert.assertEquals(lits, F.IMP3.variables());

    final Formula imp = F.f.implication(F.AND1, F.AND2);
    Assert.assertEquals(2, imp.variables().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.B));
    Assert.assertEquals(lits, imp.variables());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(4, F.IMP3.literals().size());
    SortedSet<Literal> lits = new TreeSet<Literal>(Arrays.asList(F.A, F.B, F.X, F.Y));
    Assert.assertEquals(lits, F.IMP3.literals());

    Formula imp = F.f.implication(F.AND1, F.AND2);
    Assert.assertEquals(4, imp.literals().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.NA, F.NB));
    Assert.assertEquals(lits, imp.literals());

    imp = F.f.implication(F.AND1, F.A);
    Assert.assertEquals(2, imp.literals().size());
    lits = new TreeSet<Literal>(Arrays.asList(F.A, F.B));
    Assert.assertEquals(lits, imp.literals());
  }

  @Test
  public void testNegation() {
    Assert.assertEquals(F.f.not(F.IMP1), F.IMP1.negate());
    Assert.assertEquals(F.f.not(F.IMP2), F.IMP2.negate());
    Assert.assertEquals(F.f.not(F.IMP3), F.IMP3.negate());
    Assert.assertEquals(F.f.not(F.IMP4), F.IMP4.negate());
  }

  @Test
  public void testToString() {
    Assert.assertEquals("a => b", F.IMP1.toString());
    Assert.assertEquals("~a => ~b", F.IMP2.toString());
    Assert.assertEquals("a & b => x | y", F.IMP3.toString());
    Assert.assertEquals("(a <=> b) => (~x <=> ~y)", F.IMP4.toString());
  }

  @Test
  public void testEquals() {
    Assert.assertEquals(F.IMP1, F.f.implication(F.A, F.B));
    Assert.assertEquals(F.IMP3, F.f.implication(F.AND1, F.OR1));
    Assert.assertEquals(F.IMP2, F.IMP2);
    Assert.assertNotEquals(F.IMP1, null);
    Assert.assertNotEquals(F.IMP1, F.A);
    Assert.assertNotEquals(F.IMP1, F.IMP2);
  }

  @Test
  public void testEqualsDifferentFormulaFactory() {
    Assert.assertEquals(F.IMP1, F.g.implication(F.g.variable("a"), F.g.variable("b")));
    Assert.assertEquals(F.IMP3, F.g.implication(F.AND1, F.OR1));
    Assert.assertNotEquals(F.IMP1, F.g.variable("a"));
    Assert.assertNotEquals(F.IMP1, F.g.implication(F.g.variable("b"), F.g.variable("a")));
    Assert.assertNotEquals(F.IMP1, F.g.implication(F.g.literal("a", false), F.g.variable("b")));
    Assert.assertNotEquals(F.IMP1, F.g.implication(F.g.variable("a"), F.g.literal("b", false)));
  }

  @Test
  public void testHash() {
    final Formula imp = F.f.implication(F.NA, F.NB);
    Assert.assertEquals(F.IMP2.hashCode(), imp.hashCode());
    Assert.assertEquals(F.IMP2.hashCode(), imp.hashCode());
    Assert.assertEquals(F.IMP3.hashCode(), F.f.implication(F.AND1, F.OR1).hashCode());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(2, F.IMP1.numberOfAtoms());
    Assert.assertEquals(4, F.IMP3.numberOfAtoms());
    Assert.assertEquals(4, F.IMP3.numberOfAtoms());
  }

  @Test
  public void testNumberOfNodes() {
    Assert.assertEquals(3, F.IMP1.numberOfNodes());
    Assert.assertEquals(7, F.IMP4.numberOfNodes());
    Assert.assertEquals(7, F.IMP4.numberOfNodes());
  }

  @Test
  public void testNumberOfInternalNodes() throws ParserException {
    final Formula imp = new PropositionalParser(F.f).parse("a & (b | c) => (d <=> (b | c))");
    Assert.assertEquals(7, F.IMP4.numberOfInternalNodes());
    Assert.assertEquals(8, imp.numberOfInternalNodes());
  }

  @Test
  public void testNumberOfOperands() {
    Assert.assertEquals(2, F.IMP1.numberOfOperands());
    Assert.assertEquals(2, F.IMP3.numberOfOperands());
    Assert.assertEquals(2, F.IMP4.numberOfOperands());
  }

  @Test
  public void testIsConstantFormula() {
    Assert.assertFalse(F.IMP1.isConstantFormula());
    Assert.assertFalse(F.IMP2.isConstantFormula());
    Assert.assertFalse(F.IMP3.isConstantFormula());
    Assert.assertFalse(F.IMP4.isConstantFormula());
  }

  @Test
  public void testAtomicFormula() {
    Assert.assertFalse(F.IMP1.isAtomicFormula());
    Assert.assertFalse(F.IMP4.isAtomicFormula());
  }

  @Test
  public void testContains() {
    Assert.assertTrue(F.IMP4.containsVariable(F.f.variable("a")));
    Assert.assertFalse(F.IMP4.containsVariable(F.f.variable("c")));
  }
}
