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

package org.logicng.predicates.satisfiability;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaPredicate;
import org.logicng.formulas.Variable;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.testutils.PigeonHoleGenerator;

import static org.logicng.formulas.cache.PredicateCacheEntry.IS_SAT;
import static org.logicng.formulas.cache.PredicateCacheEntry.IS_TAUTOLOGY;

/**
 * Unit tests for the different satisfiability predicates.
 * @version 1.1
 * @since 1.0
 */
public class PredicatesTest {

  private final FormulaFactory f = new FormulaFactory();
  private final FormulaPredicate sat = new SATPredicate(f);
  private final FormulaPredicate ctr = new ContradictionPredicate(f);
  private final FormulaPredicate tau = new TautologyPredicate(f);
  private final FormulaPredicate con = new ContingencyPredicate(f);

  @Test
  public void testTrue() {
    Assert.assertTrue(F.TRUE.holds(sat));
    Assert.assertFalse(F.TRUE.holds(ctr));
    Assert.assertTrue(F.TRUE.holds(tau));
    Assert.assertFalse(F.TRUE.holds(con));
  }

  @Test
  public void testFalse() {
    Assert.assertFalse(F.FALSE.holds(sat));
    Assert.assertTrue(F.FALSE.holds(ctr));
    Assert.assertFalse(F.FALSE.holds(tau));
    Assert.assertFalse(F.FALSE.holds(con));
  }

  @Test
  public void testLiterals() {
    Assert.assertTrue(F.A.holds(sat));
    Assert.assertFalse(F.A.holds(ctr));
    Assert.assertFalse(F.A.holds(tau));
    Assert.assertTrue(F.A.holds(con));
    Assert.assertTrue(F.NA.holds(sat));
    Assert.assertFalse(F.NA.holds(ctr));
    Assert.assertFalse(F.NA.holds(tau));
    Assert.assertTrue(F.NA.holds(con));
  }

  @Test
  public void testOther() {
    Assert.assertTrue(F.AND1.holds(sat));
    Assert.assertFalse(F.AND1.holds(ctr));
    Assert.assertFalse(F.AND1.holds(tau));
    Assert.assertTrue(F.AND1.holds(con));
    Assert.assertTrue(F.NOT2.holds(sat));
    Assert.assertFalse(F.NOT2.holds(ctr));
    Assert.assertFalse(F.NOT2.holds(tau));
    Assert.assertTrue(F.NOT2.holds(con));
  }

  @Test
  public void testTaut() {
    final FormulaFactory f = F.f;
    final Formula taut = f.or(F.AND1, f.and(F.NA, F.B), f.and(F.A, F.NB), f.and(F.NA, F.NB));
    Assert.assertTrue(taut.holds(sat));
    Assert.assertFalse(taut.holds(ctr));
    Assert.assertTrue(taut.holds(tau));
    Assert.assertFalse(taut.holds(con));
  }

  @Test
  public void testCont() {
    final FormulaFactory f = F.f;
    final Formula cont = f.and(F.OR1, f.or(F.NX, F.Y), f.or(F.X, F.NY), f.or(F.NX, F.NY));
    Assert.assertFalse(cont.holds(sat));
    Assert.assertTrue(cont.holds(ctr));
    Assert.assertFalse(cont.holds(tau));
    Assert.assertFalse(cont.holds(con));
  }

  @Test
  public void testSat() {
    Assert.assertTrue(F.AND1.holds(sat));
    Assert.assertTrue(F.AND2.holds(sat));
    Assert.assertTrue(F.AND3.holds(sat));
    Assert.assertTrue(F.OR1.holds(sat));
    Assert.assertTrue(F.OR2.holds(sat));
    Assert.assertTrue(F.OR3.holds(sat));
    Assert.assertTrue(F.NOT1.holds(sat));
    Assert.assertTrue(F.NOT2.holds(sat));
    Assert.assertFalse(new PigeonHoleGenerator(F.f).generate(1).holds(sat));
    Assert.assertFalse(new PigeonHoleGenerator(F.f).generate(2).holds(sat));
    Assert.assertFalse(new PigeonHoleGenerator(F.f).generate(3).holds(sat));
  }

  @Test
  public void testNotCache() {
    final FormulaFactory f = F.f;
    final Formula taut = f.or(F.AND1, f.and(F.NA, F.B), f.and(F.A, F.NB), f.and(F.NA, F.NB));
    taut.holds(tau, false);
    Assert.assertEquals(Tristate.UNDEF, taut.predicateCacheEntry(IS_TAUTOLOGY));

    Variable a = f.variable("A");
    Variable b = f.variable("B");
    Variable c = f.variable("C");
    Variable d = f.variable("D");
    final Formula satDNF = f.or(f.and(a, b), f.and(b, c), f.and(d, a));
    Assert.assertTrue(satDNF.holds(sat, false));
    Assert.assertEquals(Tristate.UNDEF, satDNF.predicateCacheEntry(IS_SAT));
  }

  @Test
  public void testToString() {
    final SATSolver s = MiniSat.miniSat(f);
    Assert.assertEquals("SATPredicate", new SATPredicate(s).toString());
    Assert.assertEquals("TautologyPredicate", new TautologyPredicate(s).toString());
    Assert.assertEquals("ContradictionPredicate", new ContradictionPredicate(s).toString());
    Assert.assertEquals("ContingencyPredicate", new ContingencyPredicate(s).toString());
  }
}
