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

package org.logicng.transformations.cnf;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.predicates.CNFPredicate;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.List;
import java.util.SortedSet;

/**
 * Unit Tests for {@link PlaistedGreenbaumTransformation}.
 * @version 1.1
 * @since 1.0
 */
public class PlaistedGreenbaumTest {

  private final PlaistedGreenbaumTransformation pg = new PlaistedGreenbaumTransformation(0);
  private final CNFPredicate cnfPredicate = new CNFPredicate();

  @Test
  public void testConstants() {
    Assert.assertEquals(F.TRUE, F.TRUE.transform(pg));
    Assert.assertEquals(F.FALSE, F.FALSE.transform(pg));
  }

  @Test
  public void testLiterals() {
    Assert.assertEquals(F.A, F.A.transform(pg));
    Assert.assertEquals(F.NA, F.NA.transform(pg));
  }

  @Test
  public void testBinaryOperators() {
    Assert.assertTrue(F.IMP1.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(F.IMP1, F.IMP1.transform(pg), F.IMP1.variables()));
    Assert.assertTrue(F.IMP2.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(F.IMP2, F.IMP2.transform(pg), F.IMP2.variables()));
    Assert.assertTrue(F.IMP3.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(F.IMP3, F.IMP3.transform(pg), F.IMP3.variables()));
    Assert.assertTrue(F.EQ1.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(F.EQ1, F.EQ1.transform(pg), F.EQ1.variables()));
    Assert.assertTrue(F.EQ2.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(F.EQ2, F.EQ2.transform(pg), F.EQ2.variables()));
    Assert.assertTrue(F.EQ3.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(F.EQ3, F.EQ3.transform(pg), F.EQ3.variables()));
    Assert.assertTrue(F.EQ4.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(F.EQ4, F.EQ4.transform(pg), F.EQ4.variables()));
  }

  @Test
  public void testNAryOperators() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(F.AND1, F.AND1.transform(pg));
    Assert.assertEquals(F.OR1, F.OR1.transform(pg));
    final Formula f1 = p.parse("(a & b & x) | (c & d & ~y)");
    final Formula f2 = p.parse("(a & b & x) | (c & d & ~y) | (~z | (c & d & ~y)) ");
    final Formula f3 = p.parse("a | b | (~x & ~y)");
    Assert.assertTrue(f1.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f1, f1.transform(pg), f1.variables()));
    Assert.assertTrue(f2.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f2, f2.transform(pg), f2.variables()));
    Assert.assertTrue(f3.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f3, f3.transform(pg), f3.variables()));
  }

  @Test
  public void testNotNary() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~a"), p.parse("~a").transform(pg));
    Assert.assertEquals(p.parse("a"), p.parse("~~a").transform(pg));
    final Formula f0 = p.parse("~(~a | b)");
    final Formula f1 = p.parse("~((a | b) | ~(x | y))");
    final Formula f2 = p.parse("~(a & b | ~a & ~b)");
    final Formula f3 = p.parse("~(~(a | b) & ~(x | y) | (a | b) & (x | y))");
    final Formula f4 = p.parse("~(a & b & ~x & ~y)");
    final Formula f5 = p.parse("~(a | b | ~x | ~y)");
    final Formula f6 = p.parse("~(a & b) & (c | (a & b))");
    Assert.assertTrue(f0.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f0, f0.transform(pg), f0.variables()));
    Assert.assertTrue(f1.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f1, f1.transform(pg), f1.variables()));
    Assert.assertTrue(f2.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f2, f2.transform(pg), f2.variables()));
    Assert.assertTrue(f3.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f3, f3.transform(pg), f3.variables()));
    Assert.assertTrue(f4.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f4, f4.transform(pg), f4.variables()));
    Assert.assertTrue(f5.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f5, f5.transform(pg), f5.variables()));
    Assert.assertTrue(f5.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f5, f5.transform(pg), f5.variables()));
    Assert.assertTrue(f6.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f6, f6.transform(pg), f6.variables()));
  }

  @Test
  public void testNotBinary() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    Assert.assertEquals(p.parse("~a"), p.parse("~a").transform(pg));
    Assert.assertEquals(p.parse("a"), p.parse("~~a").transform(pg));
    final Formula f1 = p.parse("~(~(a | b) => ~(x | y))");
    final Formula f2 = p.parse("~(a <=> b)");
    final Formula f3 = p.parse("~(~(a | b) <=> ~(x | y))");
    Assert.assertTrue(f1.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f1, f1.transform(pg), f1.variables()));
    Assert.assertTrue(f2.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f2, f2.transform(pg), f2.variables()));
    Assert.assertTrue(f3.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f3, f3.transform(pg), f3.variables()));
  }

  @Test
  public void testCC() throws ParserException {
    PseudoBooleanParser p = new PseudoBooleanParser(F.f);
    Assert.assertEquals(p.parse("a"), p.parse("a <=> (1 * b <= 1)").transform(pg));
    Assert.assertEquals(p.parse("$false"), p.parse("~(1 * b <= 1)").transform(pg));
    Assert.assertEquals(p.parse("(~b | ~c) & (~b | ~d) & (~c | ~d)"), p.parse("(1 * b + 1 * c + 1 * d <= 1)").transform(pg));
    Assert.assertEquals(p.parse("(d | @RESERVED_CC_1 | @RESERVED_CC_4) & (~@RESERVED_CC_3 | @RESERVED_CC_1 | @RESERVED_CC_4) & (~@RESERVED_CC_3 | d | @RESERVED_CC_4) & (~@RESERVED_CC_4 | @RESERVED_CC_0) & (~@RESERVED_CC_2 | @RESERVED_CC_0) & (~@RESERVED_CC_4 | ~@RESERVED_CC_2) & (c | @RESERVED_CC_3 | @RESERVED_CC_5) & (b | @RESERVED_CC_3 | @RESERVED_CC_5) & (b | c | @RESERVED_CC_5) & (~@RESERVED_CC_5 | @RESERVED_CC_2) & ~@RESERVED_CC_0"), p.parse("~(1 * b + 1 * c + 1 * d <= 1)").transform(pg));
  }

  @Test
  public void testFormulas() throws ParserException {
    final FormulaFactory fac = new FormulaFactory();
    final PlaistedGreenbaumTransformation pgNNF = new PlaistedGreenbaumTransformation(0);
    final PropositionalParser p = new PropositionalParser(fac);
    final Formula f1 = p.parse("(a | b) => c");
    final Formula f2 = p.parse("~x & ~y");
    final Formula f3 = p.parse("d & ((a | b) => c)");
    final Formula f4 = p.parse("d & ((a | b) => c) | ~x & ~y");
    Assert.assertEquals(p.parse("(@RESERVED_CNF_1 | c) & (~@RESERVED_CNF_1 | ~a) & (~@RESERVED_CNF_1 | ~b)"), f1.transform(pgNNF));
    Assert.assertEquals(p.parse("~x & ~y"), f2.transform(pgNNF));
    Assert.assertEquals(p.parse("d & @RESERVED_CNF_0 & (~@RESERVED_CNF_0 | @RESERVED_CNF_1 | c) & " +
            "(~@RESERVED_CNF_1 | ~a) & (~@RESERVED_CNF_1 | ~b)"), f3.transform(pgNNF));
    Assert.assertEquals(p.parse("(@RESERVED_CNF_2 | @RESERVED_CNF_4) & (~@RESERVED_CNF_2 | d) & " +
            "(~@RESERVED_CNF_2 | @RESERVED_CNF_0) & (~@RESERVED_CNF_0 | @RESERVED_CNF_1 | c) & " +
            "(~@RESERVED_CNF_1 | ~a) & (~@RESERVED_CNF_1 | ~b) & (~@RESERVED_CNF_4 | ~x) & " +
            "(~@RESERVED_CNF_4 | ~y)"), f4.transform(pgNNF));
    Assert.assertTrue(f1.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f1, f1.transform(pg), f1.variables()));
    Assert.assertTrue(f2.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f2, f2.transform(pg), f2.variables()));
    Assert.assertTrue(f3.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f3, f3.transform(pg), f3.variables()));
    Assert.assertTrue(f4.transform(pg, false).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f4, f4.transform(pg, false), f4.variables()));
    Assert.assertTrue(f4.transform(pg).holds(cnfPredicate));
    Assert.assertTrue(equivalentModels(f4, f4.transform(pg), f4.variables()));
  }

  @Test
  public void testFactorization() throws ParserException {
    PropositionalParser p = new PropositionalParser(F.f);
    final PlaistedGreenbaumTransformation pgf = new PlaistedGreenbaumTransformation();
    final Formula f1 = p.parse("(a | b) => c");
    final Formula f2 = p.parse("~x & ~y");
    final Formula f3 = p.parse("d & ((a | b) => c)");
    final Formula f4 = p.parse("d & ((a | b) => c) | ~x & ~y");
    Assert.assertTrue(f1.transform(pgf).holds(cnfPredicate));
    Assert.assertEquals(f1.variables().size(), f1.transform(pgf).variables().size());
    Assert.assertTrue(f2.transform(pgf).holds(cnfPredicate));
    Assert.assertEquals(f2.variables().size(), f2.transform(pgf).variables().size());
    Assert.assertTrue(f3.transform(pgf).holds(cnfPredicate));
    Assert.assertEquals(f3.variables().size(), f3.transform(pgf).variables().size());
    Assert.assertTrue(f4.transform(pgf).holds(cnfPredicate));
    Assert.assertEquals(f4.variables().size(), f4.transform(pgf).variables().size());
  }

  @Test
  public void testToString() {
    PlaistedGreenbaumTransformation pGTransformation = new PlaistedGreenbaumTransformation(5);
    Assert.assertEquals("PlaistedGreenbaumTransformation{boundary=5}", pGTransformation.toString());
  }

  private boolean equivalentModels(final Formula f1, final Formula f2, final SortedSet<Variable> vars) {
    final SATSolver s = MiniSat.miniSat(f1.factory());
    s.add(f1);
    final List<Assignment> models1 = s.enumerateAllModels(vars);
    s.reset();
    s.add(f2);
    final List<Assignment> models2 = s.enumerateAllModels(vars);
    if (models1.size() != models2.size())
      return false;
    for (final Assignment model : models1)
      if (!models2.contains(model))
        return false;
    return true;
  }
}
