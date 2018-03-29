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

import org.junit.Before;
import org.junit.Test;
import org.logicng.bdds.datastructures.BDD;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link BDDFactory} operations.
 * @version 1.4
 * @since 1.4
 */
public class BDDOperationsTest {

  private FormulaFactory f;
  private BDDFactory factory;
  private BDD bddVerum;
  private BDD bddFalsum;
  private BDD bddPosLit;
  private BDD bddNegLit;
  private BDD bddImpl;
  private BDD bddEquiv;
  private BDD bddOr;
  private BDD bddAnd;

  @Before
  public void init() throws ParserException {
    f = new FormulaFactory();
    final PropositionalParser parser = new PropositionalParser(f);
    factory = new BDDFactory(1000, 1000, f);
    factory.setNumberOfVars(3);
    bddVerum = factory.build(f.verum());
    bddFalsum = factory.build(f.falsum());
    bddPosLit = factory.build(f.literal("A", true));
    bddNegLit = factory.build(f.literal("A", false));
    bddImpl = factory.build(parser.parse("A => ~B"));
    bddEquiv = factory.build(parser.parse("A <=> ~B"));
    bddOr = factory.build(parser.parse("A | B | ~C"));
    bddAnd = factory.build(parser.parse("A & B & ~C"));
  }

  @Test
  public void testRestriction() {
    final Literal a = f.literal("A", true);
    final List<Literal> resNotA = Collections.singletonList(f.literal("A", false));
    final List<Literal> resAB = Arrays.asList(f.literal("A", true), f.literal("B", true));
    assertThat(bddVerum.restrict(a)).isEqualTo(bddVerum);
    assertThat(bddVerum.restrict(resNotA)).isEqualTo(bddVerum);
    assertThat(bddVerum.restrict(resAB)).isEqualTo(bddVerum);
    assertThat(bddFalsum.restrict(a)).isEqualTo(bddFalsum);
    assertThat(bddFalsum.restrict(resNotA)).isEqualTo(bddFalsum);
    assertThat(bddFalsum.restrict(resAB)).isEqualTo(bddFalsum);
    assertThat(bddPosLit.restrict(a)).isEqualTo(bddVerum);
    assertThat(bddPosLit.restrict(resNotA)).isEqualTo(bddFalsum);
    assertThat(bddPosLit.restrict(resAB)).isEqualTo(bddVerum);
    assertThat(bddNegLit.restrict(a)).isEqualTo(bddFalsum);
    assertThat(bddNegLit.restrict(resNotA)).isEqualTo(bddVerum);
    assertThat(bddNegLit.restrict(resAB)).isEqualTo(bddFalsum);
    assertThat(bddImpl.restrict(a)).isEqualTo(factory.build(f.literal("B", false)));
    assertThat(bddImpl.restrict(resNotA)).isEqualTo(bddVerum);
    assertThat(bddImpl.restrict(resAB)).isEqualTo(bddFalsum);
    assertThat(bddEquiv.restrict(a)).isEqualTo(factory.build(f.literal("B", false)));
    assertThat(bddEquiv.restrict(resNotA)).isEqualTo(factory.build(f.literal("B", true)));
    assertThat(bddEquiv.restrict(resAB)).isEqualTo(bddFalsum);
    assertThat(bddOr.restrict(a)).isEqualTo(bddVerum);
    assertThat(bddOr.restrict(resNotA)).isEqualTo(factory.build(f.or(f.literal("B", true), f.literal("C", false))));
    assertThat(bddOr.restrict(resAB)).isEqualTo(bddVerum);
    assertThat(bddAnd.restrict(a)).isEqualTo(factory.build(f.and(f.literal("B", true), f.literal("C", false))));
    assertThat(bddAnd.restrict(resNotA)).isEqualTo(bddFalsum);
    assertThat(bddAnd.restrict(resAB)).isEqualTo(factory.build(f.literal("C", false)));
  }
}
