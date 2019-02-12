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

import static org.logicng.formulas.F.OR1;

/**
 * Unit Tests for the class {@link And}.
 * @version 1.1
 * @since 1.0
 */
public class AndTest {

  @Test
  public void testType() {
    Assert.assertEquals(FType.AND, F.AND1.type());
  }

  @Test
  public void testCreator() {
    Assert.assertEquals(F.TRUE, F.f.and());
    Assert.assertEquals(F.TRUE, F.f.and(F.TRUE));
    Assert.assertEquals(F.FALSE, F.f.and(F.FALSE));
    Assert.assertEquals(F.FALSE, F.f.and(F.TRUE, F.FALSE));
    Assert.assertEquals(F.FALSE, F.f.and(F.FALSE, F.TRUE));
    Assert.assertEquals(F.NA, F.f.and(F.NA));
    Assert.assertEquals(F.AND1, F.f.and(F.A, F.B, F.A, F.B, F.A));
    Assert.assertEquals(F.AND1, F.f.and(F.f.and(F.A, F.B), F.A, F.f.and(F.B, F.A)));
    Assert.assertEquals(F.AND1, F.f.and(F.TRUE, F.A, F.B, F.TRUE));
    Assert.assertEquals(F.NA, F.f.and(F.NA, F.NA, F.NA));
    Assert.assertEquals(F.NA, F.f.and(F.NA, F.NA, F.TRUE, F.TRUE));
    Assert.assertEquals(F.FALSE, F.f.and(F.NA, F.NA, F.FALSE, F.TRUE));
    final List<Literal> lits = new LinkedList<>();
    lits.add(F.A);
    lits.add(F.B);
    Assert.assertEquals(F.AND1, F.f.and(lits));
    Assert.assertEquals(F.FALSE, F.f.and(F.A, F.B, F.X, F.FALSE));
    Assert.assertEquals(F.f.and(F.A, F.B, F.X, F.Y), F.f.and(F.f.and(F.A, F.B), F.f.and(F.X, F.Y)));
    Assert.assertEquals(F.AND3, F.f.cnf(F.f.clause(F.X, F.Y), F.f.and(F.f.or(F.f.and(F.NX, F.NX), F.NY), F.f.or(F.f.and(F.NX, F.TRUE), F.NY))));
    Assert.assertEquals(F.AND1, F.f.naryOperator(FType.AND, F.A, F.B, F.A, F.B, F.A));
    Assert.assertEquals(F.AND1, F.f.naryOperator(FType.AND, Arrays.asList(F.A, F.B, F.A, F.B, F.A)));
  }

  @Test
  public void testComplementaryCheck() {
    Assert.assertEquals(F.FALSE, F.f.and(F.A, F.NA));
    Assert.assertEquals(F.FALSE, F.f.and(F.A, F.B, F.f.and(F.C, F.X, F.NB)));
    Assert.assertEquals(F.FALSE, F.f.and(F.A, F.B, F.f.and(F.NX, F.B, F.X)));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testIllegalCreation() {
    F.f.naryOperator(FType.EQUIV, F.A, F.B, F.C);
  }

  @Test
  public void testVariables() {
    Assert.assertEquals(2, F.AND2.variables().size());
    SortedSet<Variable> lits = new TreeSet<>(Arrays.asList(F.A, F.B));
    Assert.assertEquals(lits, F.AND2.variables());

    final Formula and = F.f.and(F.A, F.A, F.B, F.IMP3);
    Assert.assertEquals(4, and.variables().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
    Assert.assertEquals(lits, and.variables());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(2, F.AND2.literals().size());
    SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.NA, F.NB));
    Assert.assertEquals(lits, F.AND2.literals());

    final Formula and = F.f.and(F.A, F.A, F.B, F.f.implication(F.NA, F.NB));
    Assert.assertEquals(4, and.literals().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.NA, F.B, F.NB));
    Assert.assertEquals(lits, and.literals());
  }

  @Test
  public void testToString() {
    final FormulaFactory f = new FormulaFactory();
    Assert.assertEquals("a & b", F.AND1.toString());
    Assert.assertEquals("~a & ~b", F.AND2.toString());
    Assert.assertEquals("(x | y) & (~x | ~y)", F.AND3.toString());
    Assert.assertEquals("a & b & ~x & ~y", f.and(F.A, F.B, F.NX, F.NY).toString());
    Assert.assertEquals("(a => b) & (~a => ~b)", f.and(F.IMP1, F.IMP2).toString());
    Assert.assertEquals("(a <=> b) & (~a <=> ~b)", f.and(F.EQ1, F.EQ2).toString());
  }

  @Test
  public void testEquals() {
    Assert.assertEquals(F.AND1, F.f.and(F.A, F.B));
    Assert.assertEquals(F.AND3, F.f.and(OR1, F.OR2));
    Assert.assertEquals(F.AND2, F.AND2);
    Assert.assertEquals(F.f.and(F.f.or(F.f.variable("a"), F.f.variable("b")), F.f.or(F.f.variable("x"), F.f.literal("y", false))),
            F.f.and(F.f.or(F.f.literal("y", false), F.f.variable("x")), F.f.or(F.f.variable("b"), F.f.variable("a"))));
    Assert.assertEquals(F.f.and(F.A, F.NB, OR1, F.NX), F.f.and(F.NX, F.A, F.NB, OR1));
    Assert.assertNotEquals(F.AND1, null);
    Assert.assertNotEquals(F.AND1, F.A);
    Assert.assertNotEquals(F.AND1, F.AND2);
    Assert.assertNotEquals(F.AND1, F.f.and(F.A, F.B, F.C));
  }

  @Test
  public void testEqualsDifferentFormulaFactory() {
    Assert.assertEquals(F.AND1, F.g.and(F.g.variable("a"), F.g.variable("b")));
    Assert.assertEquals(F.AND3, F.g.and(OR1, F.OR2));
    Assert.assertEquals(F.f.and(F.f.or(F.f.variable("a"), F.f.variable("b")), F.f.or(F.f.variable("x"), F.f.literal("y", false))),
            F.g.and(F.g.or(F.g.literal("y", false), F.g.variable("x")), F.f.or(F.g.variable("b"), F.g.variable("a"))));
    Assert.assertEquals(F.f.and(F.A, F.NB, OR1, F.NX), F.g.and(F.g.literal("x", false), F.g.variable("a"), F.g.literal("b", false), F.g.or(F.g.variable("x"), F.g.variable("y"))));
    Assert.assertNotEquals(F.AND1, F.g.variable("a"));
    Assert.assertNotEquals(F.AND1, F.g.and(F.g.literal("a", false), F.g.variable("b")));
    Assert.assertNotEquals(F.AND1, F.g.and(F.g.variable("a"), F.g.literal("b", false)));
    Assert.assertNotEquals(F.AND1, F.f.and(F.A, F.B, F.g.variable("c")));
  }

  @Test
  public void testHash() {
    final Formula and = F.f.and(OR1, F.OR2);
    Assert.assertEquals(F.AND3.hashCode(), and.hashCode());
    Assert.assertEquals(F.AND3.hashCode(), and.hashCode());
    Assert.assertEquals(F.AND2.hashCode(), F.f.and(F.NA, F.NB).hashCode());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(2, F.AND1.numberOfAtoms());
    Assert.assertEquals(2, F.AND2.numberOfAtoms());
    Assert.assertEquals(4, F.AND3.numberOfAtoms());
    Assert.assertEquals(4, F.AND3.numberOfAtoms());
  }

  @Test
  public void testNumberOfNodes() {
    Assert.assertEquals(3, F.AND1.numberOfNodes());
    Assert.assertEquals(3, F.AND2.numberOfNodes());
    Assert.assertEquals(7, F.AND3.numberOfNodes());
    Assert.assertEquals(7, F.AND3.numberOfNodes());
  }

  @Test
  public void testNumberOfInternalNodes() throws ParserException {
    final Formula and = new PropositionalParser(F.f).parse("a & (b | c) => (d <=> (b | c))");
    Assert.assertEquals(7, F.AND3.numberOfInternalNodes());
    Assert.assertEquals(8, and.numberOfInternalNodes());
  }

  @Test
  public void testNumberOfOperands() {
    Assert.assertEquals(2, F.AND1.numberOfOperands());
    Assert.assertEquals(2, F.AND3.numberOfOperands());
    Assert.assertEquals(3, F.f.and(F.A, F.NX, F.EQ1).numberOfOperands());
  }

  @Test
  public void testIsConstantFormula() {
    Assert.assertFalse(F.AND1.isConstantFormula());
    Assert.assertFalse(F.AND2.isConstantFormula());
  }

  @Test
  public void testAtomicFormula() {
    Assert.assertFalse(F.AND1.isAtomicFormula());
  }

  @Test
  public void testContains() throws ParserException {
    Assert.assertTrue(F.AND3.containsVariable(F.f.variable("x")));
    Assert.assertFalse(F.AND3.containsVariable(F.f.variable("a")));
    final PropositionalParser parser = new PropositionalParser(F.f);
    final Formula contAnd = parser.parse("a & b & (c | (d & e))");
    Assert.assertTrue(contAnd.containsNode(parser.parse("d & e")));
  }
}
