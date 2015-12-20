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

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link And}.
 * @author Christoph Zengler
 * @version 1.0
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
    List<Literal> lits = new LinkedList<>();
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
    SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.A, F.B));
    Assert.assertEquals(lits, F.AND2.variables());

    Formula and = F.f.and(F.A, F.A, F.B, F.IMP3);
    Assert.assertEquals(4, and.variables().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.B, F.X, F.Y));
    Assert.assertEquals(lits, and.variables());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(2, F.AND2.literals().size());
    SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.NA, F.NB));
    Assert.assertEquals(lits, F.AND2.literals());

    Formula and = F.f.and(F.A, F.A, F.B, F.f.implication(F.NA, F.NB));
    Assert.assertEquals(4, and.literals().size());
    lits = new TreeSet<>(Arrays.asList(F.A, F.NA, F.B, F.NB));
    Assert.assertEquals(lits, and.literals());
  }

  @Test
  public void testToString() {
    Assert.assertEquals("a & b", F.AND1.toString());
    Assert.assertEquals("~a & ~b", F.AND2.toString());
    Assert.assertEquals("(x | y) & (~x | ~y)", F.AND3.toString());
    Assert.assertEquals("a & b & ~x & ~y", F.f.and(F.A, F.B, F.NX, F.NY).toString());
    Assert.assertEquals("(a => b) & (~a => ~b)", F.f.and(F.IMP1, F.IMP2).toString());
    Assert.assertEquals("(a <=> b) & (~a <=> ~b)", F.f.and(F.EQ1, F.EQ2).toString());
  }

  @Test
  public void testEquals() {
    Assert.assertEquals(F.AND1, F.f.and(F.A, F.B));
    Assert.assertEquals(F.AND3, F.f.and(F.OR1, F.OR2));
    Assert.assertEquals(F.AND2, F.AND2);
    Assert.assertEquals(F.f.and(F.f.or(F.f.literal("a"), F.f.literal("b")), F.f.or(F.f.literal("x"), F.f.literal("y", false))),
            F.f.and(F.f.or(F.f.literal("y", false), F.f.literal("x")), F.f.or(F.f.literal("b"), F.f.literal("a"))));
    Assert.assertEquals(F.f.and(F.A, F.NB, F.OR1, F.NX), F.f.and(F.NX, F.A, F.NB, F.OR1));
    Assert.assertNotEquals(F.AND1, null);
    Assert.assertNotEquals(F.AND1, F.A);
    Assert.assertNotEquals(F.AND1, F.AND2);
    Assert.assertNotEquals(F.AND1, F.f.and(F.A, F.B, F.C));
  }

  @Test
  public void testHash() {
    Formula and = F.f.and(F.OR1, F.OR2);
    Assert.assertEquals(F.AND3.hashCode(), and.hashCode());
    Assert.assertEquals(F.AND3.hashCode(), and.hashCode());
    Assert.assertEquals(F.AND2.hashCode(), F.f.and(F.NA, F.NB).hashCode());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(2, F.AND1.numberOfAtoms());
    Assert.assertEquals(2, F.AND2.numberOfAtoms());
    Assert.assertEquals(4, F.AND3.numberOfAtoms());
  }
}