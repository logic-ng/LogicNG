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
//  Copyright 2015-20xx Christoph Zengler                                //
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for the assume functionality of the MiniSat style SAT solvers.
 * @version 2.0.0
 * @since 1.0
 */
public class AssumeTest {

    private final FormulaFactory f;
    private final SATSolver[] solvers;
    private final PropositionalParser parser;

    public AssumeTest() {
        this.f = new FormulaFactory();
        this.parser = new PropositionalParser(this.f);
        this.solvers = new SATSolver[6];
        this.solvers[0] = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(true).build());
        this.solvers[1] = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(false).build());
        this.solvers[2] = MiniSat.glucose(this.f, MiniSatConfig.builder().incremental(true).build(),
                GlucoseConfig.builder().build());
        this.solvers[3] = MiniSat.glucose(this.f, MiniSatConfig.builder().incremental(false).build(),
                GlucoseConfig.builder().build());
        this.solvers[4] = MiniSat.miniCard(this.f, MiniSatConfig.builder().incremental(true).build());
        this.solvers[5] = MiniSat.miniCard(this.f, MiniSatConfig.builder().incremental(false).build());
    }

    @Test
    public void testAssume() throws ParserException {
        final List<Literal> assumptions1 = Arrays.asList(this.f.literal("c", true), this.f.literal("d", true));
        final List<Literal> assumptions2 = Arrays.asList(this.f.literal("x", false), this.f.literal("y", true), this.f.literal("d", true));
        final List<Literal> assumptions3 = Arrays.asList(this.f.literal("a", false), this.f.literal("c", true), this.f.literal("a", false));
        final List<Literal> assumptions4 = Arrays.asList(this.f.literal("c", false), this.f.literal("d", true));
        final List<Literal> assumptions5 = Arrays.asList(this.f.literal("x", true), this.f.literal("x", false));
        final List<Literal> assumptions6 = Arrays.asList(this.f.literal("a", true), this.f.literal("a", false));
        for (final SATSolver s : this.solvers) {
            s.add(this.parser.parse("~a"));
            s.add(this.parser.parse("b"));
            s.add(this.parser.parse("b => c"));
            s.add(this.parser.parse("c => d"));
            s.add(this.parser.parse("d => e"));
            s.add(this.parser.parse("e => f"));
            assertThat(s.sat(this.f.literal("a", false))).isEqualTo(TRUE);
            assertThat(s.sat(this.f.variable("b"))).isEqualTo(TRUE);
            assertThat(s.sat(this.f.variable("c"))).isEqualTo(TRUE);
            assertThat(s.sat(this.f.variable("d"))).isEqualTo(TRUE);
            assertThat(s.sat(this.f.variable("e"))).isEqualTo(TRUE);
            assertThat(s.sat(this.f.variable("f"))).isEqualTo(TRUE);
            assertThat(s.sat(this.f.variable("g"))).isEqualTo(TRUE);
            assertThat(s.sat(this.f.variable("a"))).isEqualTo(FALSE);
            assertThat(s.sat(this.f.literal("b", false))).isEqualTo(FALSE);
            assertThat(s.sat(this.f.literal("c", false))).isEqualTo(FALSE);
            assertThat(s.sat(this.f.literal("d", false))).isEqualTo(FALSE);
            assertThat(s.sat(this.f.literal("e", false))).isEqualTo(FALSE);
            assertThat(s.sat(this.f.literal("f", false))).isEqualTo(FALSE);
            assertThat(s.sat(this.f.literal("g", false))).isEqualTo(TRUE);
            assertThat(s.sat(assumptions1)).isEqualTo(TRUE);
            assertThat(s.sat(assumptions2)).isEqualTo(TRUE);
            assertThat(s.sat(assumptions3)).isEqualTo(TRUE);
            assertThat(s.sat(assumptions4)).isEqualTo(FALSE);
            assertThat(s.sat(assumptions5)).isEqualTo(FALSE);
            assertThat(s.sat(assumptions6)).isEqualTo(FALSE);
            s.reset();
        }
    }
}
