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

package org.logicng.bdds;

import org.junit.Test;
import org.logicng.bdds.datastructures.BDD;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.predicates.CNFPredicate;
import org.logicng.testutils.NQueensGenerator;
import org.logicng.testutils.PigeonHoleGenerator;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Some more extensive tests for BDDs
 * @version 1.4.0
 * @since 1.4.0
 */
public class LargeBDDTest {

  @Test
  public void testPigeonHole() {
    final FormulaFactory f = new FormulaFactory();
    final PigeonHoleGenerator generator = new PigeonHoleGenerator(f);
    testPigeonHole(f, generator, 2);
    testPigeonHole(f, generator, 3);
    testPigeonHole(f, generator, 4);
    testPigeonHole(f, generator, 5);
    testPigeonHole(f, generator, 6);
    testPigeonHole(f, generator, 7);
    testPigeonHole(f, generator, 8);
    testPigeonHole(f, generator, 9);
  }

  private void testPigeonHole(final FormulaFactory f, final PigeonHoleGenerator generator, final int size) {
    final Formula pigeon = generator.generate(size);
    final BDDFactory bddFactory = new BDDFactory(10000, 10000, f);
    bddFactory.setNumberOfVars(pigeon.variables().size());
    final BDD bdd = bddFactory.build(pigeon);
    assertThat(bdd.isContradiction()).isTrue();
  }

  @Test
  public void testQueens() {
    final FormulaFactory f = new FormulaFactory();
    final NQueensGenerator generator = new NQueensGenerator(f);
    testQueens(f, generator, 4, 2);
    testQueens(f, generator, 5, 10);
    testQueens(f, generator, 6, 4);
    testQueens(f, generator, 7, 40);
    testQueens(f, generator, 8, 92);
  }

  private void testQueens(final FormulaFactory f, final NQueensGenerator generator, final int size, final int models) {
    final Formula queens = generator.generate(size);
    final BDDFactory bddFactory = new BDDFactory(10000, 10000, f);
    bddFactory.setNumberOfVars(queens.variables().size());
    final BDD bdd = bddFactory.build(queens);
    final Formula cnf = bdd.cnf();
    assertThat(cnf.holds(new CNFPredicate())).isTrue();
    final BDD cnfBDD = bddFactory.build(cnf);
    assertThat(cnfBDD).isEqualTo(bdd);
    assertThat(bdd.support()).isEqualTo(queens.variables());
    assertThat(bdd.modelCount()).isEqualTo(new BigDecimal(models));
  }
}
