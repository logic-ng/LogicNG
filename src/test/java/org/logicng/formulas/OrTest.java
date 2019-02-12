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
import java.util.LinkedList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link Or}.
 * @version 1.1
 * @since 1.0
 */
public class OrTest {

  @Test
  public void testType() {
    Assert.assertEquals(FType.OR, F.OR1.type());
  }

  @Test
  public void testCreator() {
    Assert.assertEquals(F.FALSE, F.f.or());
    Assert.assertEquals(F.TRUE, F.f.or(F.TRUE));
    Assert.assertEquals(F.FALSE, F.f.or(F.FALSE));
    Assert.assertEquals(F.TRUE, F.f.or(F.TRUE, F.FALSE));
    Assert.assertEquals(F.TRUE, F.f.or(F.FALSE, F.TRUE));
    Assert.assertEquals(F.NA, F.f.or(F.NA));
    Assert.assertEquals(F.OR1, F.f.or(F.X, F.Y, F.X, F.Y, F.X));
    Assert.assertEquals(F.OR1, F.f.or(F.f.or(F.X, F.Y), F.X, F.f.or(F.X, F.Y)));
    Assert.assertEquals(F.OR1, F.f.or(F.FALSE, F.X, F.Y, F.FALSE));
    Assert.assertEquals(F.NA, F.f.or(F.NA, F.NA, F.NA));
    Assert.assertEquals(F.NA, F.f.or(F.NA, F.NA, F.FALSE, F.FALSE));
    Assert.assertEquals(F.TRUE, F.f.or(F.NA, F.NA, F.TRUE, F.FALSE));
    final List<Literal> lits = new LinkedList<>();
    lits.add(F.X);
    lits.add(F.Y);
    Assert.assertEquals(F.OR1, F.f.or(lits));
    Assert.assertEquals(F.TRUE, F.f.or(F.A, F.B, F.X, F.TRUE));
    Assert.assertEquals(F.f.or(F.A, F.B, F.X, F.Y), F.f.or(F.f.or(F.A, F.B), F.f.or(F.X, F.Y)));
    Assert.assertEquals(F.OR3, F.f.or(F.f.and(F.A, F.B), F.f.or(F.f.and(F.f.and(F.NA, F.NB)), F.f.and(F.f.or(F.NA, F.FALSE), F.NB))));
    Assert.assertEquals(F.OR1, F.f.naryOperator(FType.OR, Arrays.asList(F.X, F.Y, F.X, F.Y, F.X)));
  }

  @Test
  public void testComplementaryCheck() {
    Assert.assertEquals(F.TRUE, F.f.or(F.A, F.NA));
    Assert.assertEquals(F.TRUE, F.f.or(F.A, F.B, F.f.or(F.C, F.X, F.NB)));
    Assert.assertEquals(F.TRUE, F.f.or(F.A, F.B, F.f.or(F.NX, F.B, F.X)));
    Assert.assertEquals(F.OR1, F.f.or(F.X, F.Y, F.f.and(F.NX, F.B, F.X)));
  }

  @Test
  public void testVariables() {
    Assert.assertEquals(2, F.OR2.variables().size());
    SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(F.X, F.Y));
    Assert.assertEquals(lits, F.OR2.variables());

    final Formula or = F.f.or(F.A, F.A, F.B, F.IMP3);
    Assert.assertEquals(4, or.variables().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
    Assert.assertEquals(lits, or.variables());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(2, F.OR2.literals().size());
    SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.NX, F.NY));
    Assert.assertEquals(lits, F.OR2.literals());

    final Formula or = F.f.or(F.A, F.A, F.B, F.f.implication(F.NB, F.NA));
    Assert.assertEquals(4, or.literals().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.NA, F.B, F.NB));
    Assert.assertEquals(lits, or.literals());
  }

  @Test
  public void testToString() {
    final FormulaFactory f = new FormulaFactory();
    Assert.assertEquals("x | y", F.OR1.toString());
    Assert.assertEquals("~x | ~y", F.OR2.toString());
    Assert.assertEquals("a & b | ~a & ~b", F.OR3.toString());
    Assert.assertEquals("a | b | ~x | ~y", f.or(F.A, F.B, F.NX, F.NY).toString());
    Assert.assertEquals("(a => b) | (~a => ~b)", f.or(F.IMP1, F.IMP2).toString());
    Assert.assertEquals("(a <=> b) | (~a <=> ~b)", f.or(F.EQ1, F.EQ2).toString());
  }

  @Test
  public void testEquals() {
    Assert.assertEquals(F.OR1, F.f.or(F.X, F.Y));
    Assert.assertEquals(F.OR3, F.f.or(F.AND1, F.AND2));
    Assert.assertEquals(F.OR2, F.OR2);
    Assert.assertEquals(F.f.or(F.A, F.NB, F.AND1, F.NX), F.f.or(F.NX, F.A, F.NB, F.AND1));
    Assert.assertNotEquals(F.OR1, null);
    Assert.assertNotEquals(F.OR1, F.A);
    Assert.assertNotEquals(F.OR1, F.OR2);
    Assert.assertNotEquals(F.OR1, F.f.or(F.A, F.B, F.C));
  }

  @Test
  public void testEqualsDifferentFormulaFactory() {
    Assert.assertEquals(F.OR1, F.g.or(F.g.variable("x"), F.g.variable("y")));
    Assert.assertEquals(F.OR3, F.g.or(F.AND1, F.AND2));
    Assert.assertEquals(F.f.or(F.f.and(F.f.variable("a"), F.f.variable("b")), F.f.and(F.f.variable("x"), F.f.literal("y", false))),
            F.g.or(F.g.and(F.g.literal("y", false), F.g.variable("x")), F.f.and(F.g.variable("b"), F.g.variable("a"))));
    Assert.assertEquals(F.f.or(F.A, F.NB, F.AND1, F.NX), F.g.or(F.g.literal("x", false), F.g.variable("a"), F.g.literal("b", false), F.g.and(F.g.variable("a"), F.g.variable("b"))));
    Assert.assertNotEquals(F.OR1, F.g.variable("a"));
    Assert.assertNotEquals(F.OR1, F.g.or(F.g.literal("a", false), F.g.variable("b")));
    Assert.assertNotEquals(F.OR1, F.g.or(F.g.variable("a"), F.g.literal("b", false)));
    Assert.assertNotEquals(F.OR1, F.f.or(F.A, F.B, F.g.variable("c")));
  }

  @Test
  public void testHash() {
    final Formula or = F.f.or(F.AND1, F.AND2);
    Assert.assertEquals(F.OR3.hashCode(), or.hashCode());
    Assert.assertEquals(F.OR3.hashCode(), or.hashCode());
    Assert.assertEquals(F.OR2.hashCode(), F.f.or(F.NX, F.NY).hashCode());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(2, F.OR1.numberOfAtoms());
    Assert.assertEquals(2, F.OR2.numberOfAtoms());
    Assert.assertEquals(4, F.OR3.numberOfAtoms());
    Assert.assertEquals(4, F.OR3.numberOfAtoms());
  }

  @Test
  public void testNumberOfNodes() {
    Assert.assertEquals(3, F.OR1.numberOfNodes());
    Assert.assertEquals(3, F.OR2.numberOfNodes());
    Assert.assertEquals(7, F.OR3.numberOfNodes());
    Assert.assertEquals(7, F.OR3.numberOfNodes());
  }

  @Test
  public void testNumberOfInternalNodes() throws ParserException {
    final Formula or = new PropositionalParser(F.f).parse("a & (b | c) => (d <=> (b | c))");
    Assert.assertEquals(7, F.OR3.numberOfInternalNodes());
    Assert.assertEquals(8, or.numberOfInternalNodes());
  }

  @Test
  public void testNumberOfOperands() {
    Assert.assertEquals(2, F.OR1.numberOfOperands());
    Assert.assertEquals(2, F.OR3.numberOfOperands());
    Assert.assertEquals(3, F.f.or(F.A, F.NX, F.EQ1).numberOfOperands());
  }

  @Test
  public void testIsConstantFormula() {
    Assert.assertFalse(F.OR1.isConstantFormula());
    Assert.assertFalse(F.OR2.isConstantFormula());
  }

  @Test
  public void testAtomicFormula() {
    Assert.assertFalse(F.OR1.isAtomicFormula());
  }

  @Test
  public void testContains() throws ParserException {
    Assert.assertTrue(F.OR1.containsVariable(F.f.variable("x")));
    Assert.assertFalse(F.OR1.containsVariable(F.f.variable("a")));
    final PropositionalParser parser = new PropositionalParser(F.f);
    final Formula contAnd = parser.parse("a | b | (c & (d | e))");
    Assert.assertTrue(contAnd.containsNode(parser.parse("d | e")));
  }
}
