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

package org.logicng.solvers.sat;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.Arrays;
import java.util.List;

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;

/**
 * Unit tests for the assume functionality of the MiniSat style SAT solvers.
 * @version 1.1
 * @since 1.0
 */
public class AssumeTest {

  private final FormulaFactory f;
  private final SATSolver[] solvers;
  private final PropositionalParser parser;

  public AssumeTest() {
    this.f = new FormulaFactory();
    this.parser = new PropositionalParser(f);
    this.solvers = new SATSolver[6];
    this.solvers[0] = MiniSat.miniSat(f, new MiniSatConfig.Builder().incremental(true).build());
    this.solvers[1] = MiniSat.miniSat(f, new MiniSatConfig.Builder().incremental(false).build());
    this.solvers[2] = MiniSat.glucose(f, new MiniSatConfig.Builder().incremental(true).build(),
            new GlucoseConfig.Builder().build());
    this.solvers[3] = MiniSat.glucose(f, new MiniSatConfig.Builder().incremental(false).build(),
            new GlucoseConfig.Builder().build());
    this.solvers[4] = MiniSat.miniCard(f, new MiniSatConfig.Builder().incremental(true).build());
    this.solvers[5] = MiniSat.miniCard(f, new MiniSatConfig.Builder().incremental(false).build());
  }

  @Test
  public void testAssume() throws ParserException {
    final List<Literal> assumptions1 = Arrays.asList(f.literal("c", true), f.literal("d", true));
    final List<Literal> assumptions2 = Arrays.asList(f.literal("x", false), f.literal("y", true), f.literal("d", true));
    final List<Literal> assumptions3 = Arrays.asList(f.literal("a", false), f.literal("c", true), f.literal("a", false));
    final List<Literal> assumptions4 = Arrays.asList(f.literal("c", false), f.literal("d", true));
    final List<Literal> assumptions5 = Arrays.asList(f.literal("x", true), f.literal("x", false));
    final List<Literal> assumptions6 = Arrays.asList(f.literal("a", true), f.literal("a", false));
    for (final SATSolver s : this.solvers) {
      s.add(parser.parse("~a"));
      s.add(parser.parse("b"));
      s.add(parser.parse("b => c"));
      s.add(parser.parse("c => d"));
      s.add(parser.parse("d => e"));
      s.add(parser.parse("e => f"));
      Assert.assertEquals(TRUE, s.sat(f.literal("a", false)));
      Assert.assertEquals(TRUE, s.sat(f.variable("b")));
      Assert.assertEquals(TRUE, s.sat(f.variable("c")));
      Assert.assertEquals(TRUE, s.sat(f.variable("d")));
      Assert.assertEquals(TRUE, s.sat(f.variable("e")));
      Assert.assertEquals(TRUE, s.sat(f.variable("f")));
      Assert.assertEquals(TRUE, s.sat(f.variable("g")));
      Assert.assertEquals(FALSE, s.sat(f.variable("a")));
      Assert.assertEquals(FALSE, s.sat(f.literal("b", false)));
      Assert.assertEquals(FALSE, s.sat(f.literal("c", false)));
      Assert.assertEquals(FALSE, s.sat(f.literal("d", false)));
      Assert.assertEquals(FALSE, s.sat(f.literal("e", false)));
      Assert.assertEquals(FALSE, s.sat(f.literal("f", false)));
      Assert.assertEquals(TRUE, s.sat(f.literal("g", false)));
      Assert.assertEquals(TRUE, s.sat(assumptions1));
      Assert.assertEquals(TRUE, s.sat(assumptions2));
      Assert.assertEquals(TRUE, s.sat(assumptions3));
      Assert.assertEquals(FALSE, s.sat(assumptions4));
      Assert.assertEquals(FALSE, s.sat(assumptions5));
      Assert.assertEquals(FALSE, s.sat(assumptions6));
      s.reset();
    }
  }
}
