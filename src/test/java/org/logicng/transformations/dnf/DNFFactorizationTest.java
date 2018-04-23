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

package org.logicng.transformations.dnf;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.handlers.FactorizationHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.predicates.CNFPredicate;
import org.logicng.predicates.DNFPredicate;

/**
 * Unit Tests for DNF conversion.
 * @version 1.1
 * @since 1.0
 */
public class DNFFactorizationTest {

  private final DNFFactorization dnfFactorization = new DNFFactorization();
  private final DNFPredicate dnfPredicate = new DNFPredicate();
  private final CNFPredicate cnfPredicate = new CNFPredicate();

  @Test
  public void testConstants() {
    Assert.assertEquals(F.TRUE, F.TRUE.transform(this.dnfFactorization));
    Assert.assertEquals(F.FALSE, F.FALSE.transform(this.dnfFactorization));
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(F.A, F.A.transform(this.dnfFactorization));
    Assert.assertEquals(F.NA, F.NA.transform(this.dnfFactorization));
  }

  @Test
  public void testBinaryOperators() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~a | b"), F.IMP1.transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("a | ~b"), F.IMP2.transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("~a | ~b | x | y"), F.IMP3.transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("(a & b) | (~a & ~b)"), F.EQ1.transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("(a & b) | (~a & ~b)"), F.EQ2.transform(this.dnfFactorization));
    Assert.assertTrue(F.IMP1.transform(this.dnfFactorization).holds(this.dnfPredicate));
    Assert.assertTrue(F.IMP2.transform(this.dnfFactorization).holds(this.dnfPredicate));
    Assert.assertTrue(F.IMP3.transform(this.dnfFactorization).holds(this.dnfPredicate));
    Assert.assertTrue(F.EQ1.transform(this.dnfFactorization).holds(this.dnfPredicate));
    Assert.assertFalse(F.EQ1.transform(this.dnfFactorization).holds(this.cnfPredicate));
    Assert.assertTrue(F.EQ2.transform(this.dnfFactorization).holds(this.dnfPredicate));
    Assert.assertFalse(F.EQ2.transform(this.dnfFactorization).holds(this.cnfPredicate));
  }

  @Test
  public void testNAryOperators() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(F.AND1, F.AND1.transform(this.dnfFactorization));
    Assert.assertEquals(F.OR1, F.OR1.transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("~a | ~b | c | (~x & y)"), p.parse("~(a & b) | c | ~(x | ~y)").transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("(~a & ~b & c & ~x) | (~a & ~b & c & y)"), p.parse("~(a | b) & c & ~(x & ~y)").transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("(a & b & ~x) | (a & b & ~y)"), p.parse("a & b & (~x | ~y)").transform(this.dnfFactorization));
    Assert.assertTrue(F.AND1.transform(this.dnfFactorization).holds(this.dnfPredicate));
    Assert.assertTrue(F.OR1.transform(this.dnfFactorization).holds(this.dnfPredicate));
    Assert.assertTrue(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(this.dnfFactorization).holds(this.dnfPredicate));
    Assert.assertFalse(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(this.dnfFactorization).holds(this.cnfPredicate));
    Assert.assertTrue(p.parse("~(a & b) | c | ~(x | ~y)").transform(this.dnfFactorization).holds(this.dnfPredicate));
    Assert.assertFalse(p.parse("~(a & b) | c | ~(x | ~y)").transform(this.dnfFactorization).holds(this.cnfPredicate));
    Assert.assertTrue(p.parse("a | b | (~x & ~y)").transform(this.dnfFactorization).holds(this.dnfPredicate));
    Assert.assertFalse(p.parse("a | b | (~x & ~y)").transform(this.dnfFactorization).holds(this.cnfPredicate));
  }

  @Test
  public void testNot() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~a"), p.parse("~a").transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("a"), p.parse("~~a").transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("a & ~b"), p.parse("~(a => b)").transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("(~a & ~b & x) | (~a & ~b & y)"), p.parse("~(~(a | b) => ~(x | y))").transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("(~a & b) | (a & ~b)"), p.parse("~(a <=> b)").transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("~a | ~b | x | y"), p.parse("~(a & b & ~x & ~y)").transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("~a & ~b & x & y"), p.parse("~(a | b | ~x | ~y)").transform(this.dnfFactorization));
    Assert.assertEquals(p.parse("~a & ~b & x & y"), p.parse("~(a | b | ~x | ~y)").transform(this.dnfFactorization));
  }

  @Test
  public void testCDNF() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    final Formula f = p.parse("x0 & x1 & x3 | ~x1 & ~x2 | x2 & ~x3");
    final Formula cdnf = p.parse("x0 & x1 & x2 & x3 | x0 & x1 & x2 & ~x3 | x0 & ~x1 & x2 & ~x3 | ~x0 & ~x1 & x2 & ~x3 | ~x0 & ~x1 & ~x2 & ~x3 | x0 & ~x1 & ~x2 & ~x3 | x0 & ~x1 & ~x2 & x3 | x0 & x1 & ~x2 & x3 | ~x0 & x1 & x2 & ~x3 | ~x0 & ~x1 & ~x2 & x3");
    Assert.assertEquals(cdnf, f.transform(new CanonicalDNFEnumeration()));
    Assert.assertEquals(F.f.falsum(), F.f.and(F.A, F.NA).transform(new CanonicalDNFEnumeration()));
  }

  @Test
  public void testWithHandler() throws ParserException {
    final PropositionalParser p = new PropositionalParser(F.f);
    final Formula formula = p.parse("(~(~(a | b) => ~(x | y))) & ((a | x) => ~(b | y))");
    final DNFFactorization factorization = new DNFFactorization(new FactorizationHandler() {
      private int dists = 0;
      private int clauses = 0;

      @Override
      public boolean performedDistribution() {
        this.dists++;
        return this.dists < 100;
      }

      @Override
      public boolean createdClause(final Formula clause) {
        this.clauses++;
        return this.clauses < 5;
      }
    });
    Assert.assertNull(factorization.apply(formula, false));
  }

  @Test
  public void testToString() {
    Assert.assertEquals("DNFFactorization", this.dnfFactorization.toString());
    Assert.assertEquals("CanonicalDNFEnumeration", new CanonicalDNFEnumeration().toString());
  }
}
