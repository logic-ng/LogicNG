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

package org.logicng.transformations.qmc;

import org.junit.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.transformations.qmc.QuineMcCluskey.convertToTerm;

/**
 * Unit tests for {@link QuineMcCluskey}.
 * @version 1.4.0
 * @since 1.4.0
 */
public class QuineMcCluskeyTest {

  @Test
  public void testConvertToTerm() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final List<Literal> minterm1 = Arrays.asList(f.literal("A", true), f.literal("B", true), f.literal("C", true));
    final List<Literal> minterm2 = Arrays.asList(f.literal("A", true), f.literal("B", false), f.literal("C", true));
    final List<Literal> minterm3 = Arrays.asList(f.literal("A", false), f.literal("B", false), f.literal("C", false));

    assertThat(convertToTerm(minterm1, f).bits()).isEqualTo(new Tristate[]{Tristate.TRUE, Tristate.TRUE, Tristate.TRUE});
    assertThat(convertToTerm(minterm2, f).bits()).isEqualTo(new Tristate[]{Tristate.TRUE, Tristate.FALSE, Tristate.TRUE});
    assertThat(convertToTerm(minterm3, f).bits()).isEqualTo(new Tristate[]{Tristate.FALSE, Tristate.FALSE, Tristate.FALSE});

    assertThat(convertToTerm(minterm1, f).minterms()).isEqualTo(Collections.singletonList(p.parse("A & B & C")));
    assertThat(convertToTerm(minterm2, f).minterms()).isEqualTo(Collections.singletonList(p.parse("A & ~B & C")));
    assertThat(convertToTerm(minterm3, f).minterms()).isEqualTo(Collections.singletonList(p.parse("~A & ~B & ~C")));

    assertThat(convertToTerm(minterm1, f).termClass()).isEqualTo(3);
    assertThat(convertToTerm(minterm2, f).termClass()).isEqualTo(2);
    assertThat(convertToTerm(minterm3, f).termClass()).isEqualTo(0);
  }

}
