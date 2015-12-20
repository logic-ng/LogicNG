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
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit Tests for the class {@link Not}.
 * @author Christoph Zengler
 * @version 1.0
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
    SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.A, F.B));
    Assert.assertEquals(lits, F.NOT1.variables());

    Assert.assertEquals(2, F.NOT2.variables().size());
    lits = new TreeSet<>(Arrays.asList(F.X, F.Y));
    Assert.assertEquals(lits, F.NOT2.variables());
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(2, F.NOT1.literals().size());
    SortedSet<Literal> lits = new TreeSet<>(Arrays.asList(F.A, F.B));
    Assert.assertEquals(lits, F.NOT1.literals());

    Formula not = F.f.not(F.f.and(F.A, F.NB, F.f.implication(F.B, F.NA)));
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
  public void testHash() {
    Formula not = F.f.not(F.AND1);
    Assert.assertEquals(F.NOT1.hashCode(), not.hashCode());
    Assert.assertEquals(F.NOT1.hashCode(), not.hashCode());
    Assert.assertEquals(F.NOT2.hashCode(), F.f.not(F.OR1).hashCode());
  }

  @Test
  public void testNumberOfAtoms() {
    Assert.assertEquals(2, F.NOT1.numberOfAtoms());
    Assert.assertEquals(2, F.OR1.numberOfAtoms());
  }
}
