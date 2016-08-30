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

package org.logicng.bdds.simple;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.bdds.BDD;
import org.logicng.bdds.BDDNode;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for {@link BDDFactoryClassical}.
 * @version 1.2
 * @since 1.2
 */
public class BDDFactoryTest {

  @Test
  public void test1() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula f1 = p.parse("a & b | ~c");
    final BDDFactoryClassical bdd = new BDDFactoryClassical(f, f1.variables());
    Assert.assertEquals(bdd.build(f1), new BDD(bdd.buildWithApply(f1), bdd));
  }


  @Test
  public void test2() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula f1 = p.parse("~(~a & ~b & ~x & ~y) & ~(~(~a & ~b) & ~(~x & ~y))");
    final Formula f2 = p.parse("~(~(~a & ~b & ~(~x & ~y)) & ~((a | b) & ~(x | y)))");
    final BDDFactoryClassical bdd = new BDDFactoryClassical(f, f1.variables());
    Assert.assertEquals(bdd.build(f1), bdd.build(f2));
  }

  @Test
  public void test3() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula f1 = p.parse("a & b | ~c");
    final BDDFactoryClassical bdd = new BDDFactoryClassical(f, f1.variables());
    final BDDNode bddNode = bdd.bddForFormula(f1);
    Assert.assertEquals(5, bddNode.nodes().size());
    Assert.assertEquals(f.variable("a"), bddNode.label());
  }
}
