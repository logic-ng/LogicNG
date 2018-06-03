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

package org.logicng.bdds.orderings;

import org.junit.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for {@link DFSOrdering}.
 * @version 1.4.0
 * @since 1.4.0
 */
public class DFSOrderingTest {

  private final DFSOrdering ordering = new DFSOrdering();

  @Test
  public void testSimpleCases() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PseudoBooleanParser p = new PseudoBooleanParser(f);
    assertThat(this.ordering.getOrder(p.parse("$true"))).isEmpty();
    assertThat(this.ordering.getOrder(p.parse("$false"))).isEmpty();
    assertThat(this.ordering.getOrder(p.parse("A"))).containsExactly(f.variable("A"));
    assertThat(this.ordering.getOrder(p.parse("A => ~B"))).containsExactly(f.variable("A"), f.variable("B"));
    assertThat(this.ordering.getOrder(p.parse("A <=> ~B"))).containsExactly(f.variable("A"), f.variable("B"));
    assertThat(this.ordering.getOrder(p.parse("~(A <=> ~B)"))).containsExactly(f.variable("A"), f.variable("B"));
    assertThat(this.ordering.getOrder(p.parse("A | ~C | B | D"))).containsExactly(f.variable("A"), f.variable("C"), f.variable("B"), f.variable("D"));
    assertThat(this.ordering.getOrder(p.parse("A & ~C & B & D"))).containsExactly(f.variable("A"), f.variable("C"), f.variable("B"), f.variable("D"));
    assertThat(this.ordering.getOrder(p.parse("A + C + B + D < 2"))).containsExactly(f.variable("A"), f.variable("C"), f.variable("B"), f.variable("D"));
  }

  @Test
  public void testComplexFormula() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PseudoBooleanParser p = new PseudoBooleanParser(f);
    final Formula formula = p.parse("(A => ~B) & ((A & C) | (D & ~C)) & (A | Y | X) & (Y <=> (X | (W + A + F < 1)))");
    assertThat(this.ordering.getOrder(formula)).containsExactly(
            f.variable("A"),
            f.variable("B"),
            f.variable("C"),
            f.variable("D"),
            f.variable("Y"),
            f.variable("X"),
            f.variable("W"),
            f.variable("F")
    );
  }
}
