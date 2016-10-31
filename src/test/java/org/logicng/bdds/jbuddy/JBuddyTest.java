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
//  Copyright 2015-2016 Christoph Zengler                                //
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

package org.logicng.bdds.jbuddy;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.bdds.BDD;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.predicates.CNFPredicate;
import org.logicng.predicates.satisfiability.TautologyPredicate;

import java.util.Arrays;

/**
 * Unit tests for {@link JBuddyFactory}.
 * @version 1.2
 * @since 1.2
 */
public class JBuddyTest {

  @Test
  public void test() {
    final FormulaFactory f = new FormulaFactory();
    JBuddyFactory factory = new JBuddyFactory(1000, 1000, f);
    factory.setVariableOrder(Arrays.asList(f.variable("x"), f.variable("y"), f.variable("a"), f.variable("b")));
    final Formula formula = f.or(f.and(f.literal("a", false), f.variable("b")), f.and(f.variable("x"), f.variable("y")));
    final BDD bdd = factory.build(formula);
    final Formula cnf = bdd.cnf();
    Assert.assertTrue(cnf.holds(new CNFPredicate()));
    Assert.assertTrue(f.equivalence(formula, cnf).holds(new TautologyPredicate(f)));
  }
}
