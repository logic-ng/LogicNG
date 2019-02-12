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
 * Unit Tests for the class {@link Not}.
 * @version 1.1
 * @since 1.0
 */
public class NotTest {

  @Test
  public void testType() {
    Assert.assertEquals(FType.NOT, F.NOT1.type());
  }

  @Test
  public void testCreator() {
    Assert.assertEquals(F.TRUE, F.f.not(F.FALSE));
    Assert.assertEquals(F.FALSE, F.f.not(F.TRUE));
    Assert.assertEquals(F.A, F.f.not(F.NA));
    Assert.assertEquals(F.NA, F.f.not(F.A));
    Assert.assertEquals(F.IMP3, F.f.not(F.f.not(F.IMP3)));
    Assert.assertEquals(F.NOT1, F.f.not(F.AND1));
  }

  @Test
  public void testGetters() {
    Assert.assertEquals(F.AND1, ((Not) F.NOT1).operand());
    Assert.assertEquals(F.OR1, ((Not) F.NOT2).operand());
  }

  @Test
  public void testVariables() {
    Assert.assertEquals(2, F.NOT1.variables().size());
    SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(F.A, F.B));
    Assert.assertEquals(lits, F.NOT1.variables());

    Assert.assertEquals(2, F.NOT2.variables().size());
    lits = new TreeSet<>(Arrays.asList(F.X, F.Y));
    Assert.assertEquals(lits, F.NOT2.variables());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(2, F.NOT1.literals().size());
    SortedSet<? extends Literal> lits = new TreeSet<>(Arrays.asList(F.A, F.B));
    Assert.assertEquals(lits, F.NOT1.literals());

    final Formula not = F.f.not(F.f.and(F.A, F.NB, F.f.implication(F.B, F.NA)));
    Assert.assertEquals(4, not.literals().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.NA, F.B, F.NB));
    Assert.assertEquals(lits, not.literals());
  }

  @Test
  public void testToString() {
    Assert.assertEquals("~(a & b)", F.NOT1.toString());
    Assert.assertEquals("~(x | y)", F.NOT2.toString());
  }

  @Test
  public void testEquals() {
    Assert.assertEquals(F.NOT1, F.f.not(F.AND1));
    Assert.assertEquals(F.NOT2, F.f.not(F.OR1));
    Assert.assertEquals(F.NOT1, F.NOT1);
    Assert.assertNotEquals(F.NOT1, null);
    Assert.assertNotEquals(F.NOT1, F.A);
    Assert.assertNotEquals(F.NOT1, F.NOT2);
  }

  @Test
  public void testEqualsDifferentFormulaFactory() {
    Assert.assertEquals(F.NOT1, F.g.not(F.AND1));
    Assert.assertEquals(F.NOT2, F.g.not(F.g.or(F.g.variable("x"), F.g.variable("y"))));
    Assert.assertNotEquals(F.NOT1, F.g.variable("x"));
    Assert.assertNotEquals(F.NOT2, F.g.not(F.g.or(F.g.variable("a"), F.g.variable("b"))));
  }

  @Test
  public void testHash() {
    final Formula not = F.f.not(F.AND1);
    Assert.assertEquals(F.NOT1.hashCode(), not.hashCode());
    Assert.assertEquals(F.NOT1.hashCode(), not.hashCode());
    Assert.assertEquals(F.NOT2.hashCode(), F.f.not(F.OR1).hashCode());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(2, F.NOT1.numberOfAtoms());
    Assert.assertEquals(2, F.NOT2.numberOfAtoms());
    Assert.assertEquals(2, F.OR1.numberOfAtoms());
    Assert.assertEquals(2, F.OR1.numberOfAtoms());
  }

  @Test
  public void testNumberOfNodes() {
    Assert.assertEquals(4, F.NOT1.numberOfNodes());
    Assert.assertEquals(4, F.NOT2.numberOfNodes());
    Assert.assertEquals(4, F.NOT2.numberOfNodes());
  }

  @Test
  public void testNumberOfInternalNodes() throws ParserException {
    final Formula eq = new PropositionalParser(F.f).parse("a & (b | c) <=> ~(d => (b | c))");
    Assert.assertEquals(4, F.NOT1.numberOfInternalNodes());
    Assert.assertEquals(9, eq.numberOfInternalNodes());
  }

  @Test
  public void testNumberOfOperands() {
    Assert.assertEquals(1, F.NOT1.numberOfOperands());
    Assert.assertEquals(1, F.f.not(F.EQ1).numberOfOperands());
  }

  @Test
  public void testIsConstantFormula() {
    Assert.assertFalse(F.NOT1.isConstantFormula());
    Assert.assertFalse(F.NOT2.isConstantFormula());
  }

  @Test
  public void testAtomicFormula() {
    Assert.assertFalse(F.NOT1.isAtomicFormula());
    Assert.assertFalse(F.NOT2.isAtomicFormula());
  }

  @Test
  public void testContains() {
    Assert.assertTrue(F.NOT1.containsVariable(F.f.variable("a")));
    Assert.assertFalse(F.NOT1.containsVariable(F.f.variable("x")));
  }
}
