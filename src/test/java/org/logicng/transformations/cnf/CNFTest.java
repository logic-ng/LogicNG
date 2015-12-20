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

package org.logicng.transformations.cnf;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.handlers.FactorizationHandler;
import org.logicng.io.parser.ParserException;
import org.logicng.io.parser.PropositionalParser;
import org.logicng.predicates.CNFPredicate;
import org.logicng.predicates.DNFPredicate;


/**
 * Unit Tests for CNF conversion.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public class CNFTest {

  private final FactorizationHandler handler = new TestFactorizationHandler();
  private final CNFFactorization cnf = new CNFFactorization(handler);
  private final CNFPredicate cnfPredicate = new CNFPredicate();
  private final DNFPredicate dnfPredicate = new DNFPredicate();

  @Test
  public void testConstants() {
    Assert.assertEquals(F.TRUE, F.TRUE.transform(cnf));
    Assert.assertEquals(F.FALSE, F.FALSE.transform(cnf));
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(F.A, F.A.transform(cnf));
    Assert.assertEquals(F.NA, F.NA.transform(cnf));
  }

  @Test
  public void testBinaryOperators() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~a | b"), F.IMP1.transform(cnf));
    Assert.assertEquals(p.parse("a | ~b"), F.IMP2.transform(cnf));
    Assert.assertEquals(p.parse("~a | ~b | x | y"), F.IMP3.transform(cnf));
    Assert.assertEquals(p.parse("(a | ~b) & (~a | b)"), F.EQ1.transform(cnf));
    Assert.assertEquals(p.parse("(~a | b) & (a | ~b)"), F.EQ2.transform(cnf));
    Assert.assertTrue(F.IMP1.transform(cnf).holds(cnfPredicate));
    Assert.assertTrue(F.IMP2.transform(cnf).holds(cnfPredicate));
    Assert.assertTrue(F.IMP3.transform(cnf).holds(cnfPredicate));
    Assert.assertTrue(F.EQ1.transform(cnf).holds(cnfPredicate));
    Assert.assertFalse(F.EQ1.transform(cnf).holds(dnfPredicate));
    Assert.assertTrue(F.EQ2.transform(cnf).holds(cnfPredicate));
    Assert.assertFalse(F.EQ2.transform(cnf).holds(dnfPredicate));
  }

  @Test
  public void testNAryOperators() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(F.AND1, F.AND1.transform(cnf));
    Assert.assertEquals(F.OR1, F.OR1.transform(cnf));
    Assert.assertEquals(p.parse("~a & ~b & c & (~x | y) & (~w | z)"), p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(cnf));
    Assert.assertEquals(p.parse("(~a | ~b | c | ~x) & (~a  | ~b | c | y)"), p.parse("~(a & b) | c | ~(x | ~y)").transform(cnf));
    Assert.assertEquals(p.parse("(a | b | ~x) & (a | b | ~y)"), p.parse("a | b | (~x & ~y)").transform(cnf));
    Assert.assertTrue(F.AND1.transform(cnf).holds(cnfPredicate));
    Assert.assertTrue(F.OR1.transform(cnf).holds(cnfPredicate));
    Assert.assertTrue(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(cnf).holds(cnfPredicate));
    Assert.assertFalse(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(cnf).holds(dnfPredicate));
    Assert.assertTrue(p.parse("~(a & b) | c | ~(x | ~y)").transform(cnf).holds(cnfPredicate));
    Assert.assertFalse(p.parse("~(a & b) | c | ~(x | ~y)").transform(cnf).holds(dnfPredicate));
    Assert.assertTrue(p.parse("a | b | (~x & ~y)").transform(cnf).holds(cnfPredicate));
    Assert.assertFalse(p.parse("a | b | (~x & ~y)").transform(cnf).holds(dnfPredicate));
  }

  @Test
  public void testNot() throws ParserException {
    final TestFactorizationHandler handler2 = new TestFactorizationHandler();
    final CNFFactorization cnf2 = new CNFFactorization(handler2);
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~a2"), p.parse("~a2").transform(cnf));
    Assert.assertEquals(p.parse("a2"), p.parse("~~a2").transform(cnf));
    Assert.assertEquals(p.parse("a2 & ~b2"), p.parse("~(a2 => b2)").transform(cnf));
    Assert.assertEquals(p.parse("~a2 & ~b2 & (x2 | y2)"), p.parse("~(~(a2 | b2) => ~(x2 | y2))").transform(cnf));
    Assert.assertEquals(p.parse("(~a2 | ~b2) & (a2 | b2)"), p.parse("~(a2 <=> b2)").transform(cnf));
    Assert.assertEquals(p.parse("(a2 | b2 | x2 | y2) & (~a2 | ~x2) & (~a2 | ~y2) & (~b2 | ~x2) & (~b2 | ~y2)"), p.parse("~(~(a2 | b2) <=> ~(x2 | y2))").transform(cnf2));
    Assert.assertEquals(p.parse("~a2 | ~b2 | x2 | y2"), p.parse("~(a2 & b2 & ~x2 & ~y2)").transform(cnf));
    Assert.assertEquals(p.parse("~a2 & ~b2 & x2 & y2"), p.parse("~(a2 | b2 | ~x2 | ~y2)").transform(cnf));
    Assert.assertEquals(p.parse("~a2 & ~b2 & x2 & y2"), p.parse("~(a2 | b2 | ~x2 | ~y2)").transform(cnf));
    Assert.assertEquals(7, handler2.distCount);
    Assert.assertEquals(4, handler2.clauseCount);
    Assert.assertEquals(2, handler2.longestClause);
  }

  private static class TestFactorizationHandler implements FactorizationHandler {

    private int distCount = 0;
    private int clauseCount = 0;
    private int longestClause = 0;

    @Override
    public boolean performedDistribution() {
      distCount++;
      return true;
    }

    @Override
    public boolean createdClause(final Formula clause) {
      clauseCount++;
      longestClause = Math.max(clause.numberOfAtoms(), longestClause);
      return true;
    }

    private int distCount() {
      return this.distCount;
    }

    private int clauseCount() {
      return this.clauseCount;
    }

    private int longestClause() {
      return this.longestClause;
    }
  }
}
