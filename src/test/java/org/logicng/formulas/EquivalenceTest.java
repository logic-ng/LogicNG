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

import org.junit.Assert;
import org.junit.Test;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link Equivalence}.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public class EquivalenceTest {

  @Test
  public void testType() {
    Assert.assertEquals(FType.EQUIV, F.EQ1.type());
  }

  @Test
  public void testCreator() {
    Assert.assertEquals(F.AND1, F.f.equivalence(F.TRUE, F.AND1));
    Assert.assertEquals(F.AND1, F.f.equivalence(F.AND1, F.TRUE));
    Assert.assertEquals(F.NOT1, F.f.equivalence(F.FALSE, F.AND1));
    Assert.assertEquals(F.NOT1, F.f.equivalence(F.AND1, F.FALSE));
    Assert.assertEquals(F.TRUE, F.f.equivalence(F.OR1, F.OR1));
    Assert.assertEquals(F.FALSE, F.f.equivalence(F.NOT1, F.AND1));
    Assert.assertEquals(F.FALSE, F.f.equivalence(F.AND1, F.NOT1));
    Assert.assertEquals(F.FALSE, F.f.equivalence(F.OR1, F.NOT2));
    Assert.assertEquals(F.FALSE, F.f.equivalence(F.NOT2, F.OR1));
    Assert.assertEquals(F.EQ3, F.f.binaryOperator(FType.EQUIV, F.AND1, F.OR1));
  }

  @Test
  public void testGetters() {
    Assert.assertEquals(F.NA, ((Equivalence) F.EQ2).left());
    Assert.assertEquals(F.NB, ((Equivalence) F.EQ2).right());
  }

  @Test
  public void testVariables() {
    Assert.assertEquals(4, F.IMP3.variables().size());
    SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
    Assert.assertEquals(lits, F.IMP3.variables());

    Formula equiv = F.f.equivalence(F.AND1, F.AND2);
    Assert.assertEquals(2, equiv.variables().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.B));
    Assert.assertEquals(lits, equiv.variables());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(4, F.IMP3.literals().size());
    SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
    Assert.assertEquals(lits, F.IMP3.literals());

    Formula equiv = F.f.equivalence(F.AND1, F.AND2);
    Assert.assertEquals(4, equiv.literals().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.NA, F.NB));
    Assert.assertEquals(lits, equiv.literals());

    equiv = F.f.equivalence(F.AND1, F.A);
    Assert.assertEquals(2, equiv.literals().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.B));
    Assert.assertEquals(lits, equiv.literals());
  }

  @Test
  public void testNegation() {
    Assert.assertEquals(F.f.not(F.EQ1), F.EQ1.negate());
    Assert.assertEquals(F.f.not(F.EQ2), F.EQ2.negate());
    Assert.assertEquals(F.f.not(F.EQ3), F.EQ3.negate());
    Assert.assertEquals(F.f.not(F.EQ4), F.EQ4.negate());
  }

  @Test
  public void testToString() {
    Assert.assertEquals("a <=> b", F.EQ1.toString());
    Assert.assertEquals("~a <=> ~b", F.EQ2.toString());
    Assert.assertEquals("a & b <=> x | y", F.EQ3.toString());
    Assert.assertEquals("a => b <=> ~a => ~b", F.EQ4.toString());
  }

  @Test
  public void testEquals() {
    Assert.assertEquals(F.EQ1, F.f.equivalence(F.A, F.B));
    Assert.assertEquals(F.EQ1, F.f.equivalence(F.B, F.A));
    Assert.assertEquals(F.EQ3, F.f.equivalence(F.AND1, F.OR1));
    Assert.assertEquals(F.EQ4, F.EQ4);
    Assert.assertNotEquals(F.EQ1, null);
    Assert.assertNotEquals(F.EQ1, F.A);
    Assert.assertNotEquals(F.EQ1, F.EQ2);
  }

  @Test
  public void testEqualsDifferentFormulaFactory() {
    Assert.assertEquals(F.EQ1, F.g.equivalence(F.g.literal("a"), F.g.literal("b")));
    Assert.assertEquals(F.EQ1, F.g.equivalence(F.B, F.A));
    Assert.assertEquals(F.EQ3, F.g.equivalence(F.AND1, F.OR1));
    Assert.assertNotEquals(F.EQ1, F.g.literal("a"));
    Assert.assertNotEquals(F.EQ1, F.g.equivalence(F.g.literal("a", false), F.g.literal("b")));
    Assert.assertNotEquals(F.EQ1, F.g.equivalence(F.g.literal("a"), F.g.literal("b", false)));
  }

  @Test
  public void testHash() {
    Formula eq = F.f.equivalence(F.IMP1, F.IMP2);
    Assert.assertEquals(F.EQ4.hashCode(), eq.hashCode());
    Assert.assertEquals(F.EQ4.hashCode(), eq.hashCode());
    Assert.assertEquals(F.EQ3.hashCode(), F.f.equivalence(F.AND1, F.OR1).hashCode());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(2, F.EQ1.numberOfAtoms());
    Assert.assertEquals(4, F.EQ4.numberOfAtoms());
    Assert.assertEquals(4, F.EQ4.numberOfAtoms());
  }

  @Test
  public void testNumberOfNodes() {
    Assert.assertEquals(3, F.EQ1.numberOfNodes());
    Assert.assertEquals(7, F.EQ4.numberOfNodes());
    Assert.assertEquals(7, F.EQ4.numberOfNodes());
  }

  @Test
  public void testNumberOfInternalNodes() throws ParserException {
    final Formula eq = new PropositionalParser(F.f).parse("a & (b | c) <=> (d => (b | c))");
    Assert.assertEquals(7, F.EQ4.numberOfInternalNodes());
    Assert.assertEquals(8, eq.numberOfInternalNodes());
  }

  @Test
  public void testAtomicFormula() {
    Assert.assertFalse(F.EQ1.isAtomicFormula());
    Assert.assertFalse(F.EQ4.isAtomicFormula());
  }

  @Test
  public void testContains() {
    Assert.assertTrue(F.EQ4.contains(F.f.literal("a")));
    Assert.assertFalse(F.EQ4.contains(F.f.literal("x")));
  }
}
