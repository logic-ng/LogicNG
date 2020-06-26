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

package org.logicng.transformations.qe;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;

/**
 * Unit tests for {@link UniversalQuantifierElimination} and {@link ExistentialQuantifierElimination}.
 * @version 2.0.0
 * @since 1.0
 */
public class QETest {

    private final FormulaFactory f = new FormulaFactory();
    private final PropositionalParser p = new PropositionalParser(this.f);
    private final ExistentialQuantifierElimination ex1 = new ExistentialQuantifierElimination();
    private final ExistentialQuantifierElimination ex2 = new ExistentialQuantifierElimination(this.f.variable("x"));
    private final ExistentialQuantifierElimination ex3 = new ExistentialQuantifierElimination(Arrays.asList(this.f.variable("x"), this.f.variable("y")));
    private final UniversalQuantifierElimination uni1 = new UniversalQuantifierElimination();
    private final UniversalQuantifierElimination uni2 = new UniversalQuantifierElimination(this.f.variable("x"));
    private final UniversalQuantifierElimination uni3 = new UniversalQuantifierElimination(Arrays.asList(this.f.variable("x"), this.f.variable("y")));

    @Test
    public void testConstants() {
        assertThat(this.f.verum().transform(this.ex1)).isEqualTo(this.f.verum());
        assertThat(this.f.verum().transform(this.ex2)).isEqualTo(this.f.verum());
        assertThat(this.f.verum().transform(this.ex3)).isEqualTo(this.f.verum());
        assertThat(this.f.verum().transform(this.uni1)).isEqualTo(this.f.verum());
        assertThat(this.f.verum().transform(this.uni2)).isEqualTo(this.f.verum());
        assertThat(this.f.verum().transform(this.uni3)).isEqualTo(this.f.verum());
        assertThat(this.f.falsum().transform(this.ex1)).isEqualTo(this.f.falsum());
        assertThat(this.f.falsum().transform(this.ex2)).isEqualTo(this.f.falsum());
        assertThat(this.f.falsum().transform(this.ex3)).isEqualTo(this.f.falsum());
        assertThat(this.f.falsum().transform(this.uni1)).isEqualTo(this.f.falsum());
        assertThat(this.f.falsum().transform(this.uni2)).isEqualTo(this.f.falsum());
        assertThat(this.f.falsum().transform(this.uni3)).isEqualTo(this.f.falsum());
    }

    @Test
    public void testLiterals() {
        final Formula x = this.f.variable("x");
        final Formula y = this.f.literal("y", false);
        final Formula z = this.f.variable("z");
        assertThat(x.transform(this.ex1)).isEqualTo(x);
        assertThat(x.transform(this.ex2)).isEqualTo(this.f.verum());
        assertThat(x.transform(this.ex3)).isEqualTo(this.f.verum());
        assertThat(x.transform(this.uni1)).isEqualTo(x);
        assertThat(x.transform(this.uni2)).isEqualTo(this.f.falsum());
        assertThat(x.transform(this.uni3)).isEqualTo(this.f.falsum());
        assertThat(y.transform(this.ex1)).isEqualTo(y);
        assertThat(y.transform(this.ex2)).isEqualTo(y);
        assertThat(y.transform(this.ex3)).isEqualTo(this.f.verum());
        assertThat(y.transform(this.uni1)).isEqualTo(y);
        assertThat(y.transform(this.uni2)).isEqualTo(y);
        assertThat(y.transform(this.uni3)).isEqualTo(this.f.falsum());
        assertThat(z.transform(this.ex1)).isEqualTo(z);
        assertThat(z.transform(this.ex2)).isEqualTo(z);
        assertThat(z.transform(this.ex3)).isEqualTo(z);
        assertThat(z.transform(this.uni1)).isEqualTo(z);
        assertThat(z.transform(this.uni2)).isEqualTo(z);
        assertThat(z.transform(this.uni3)).isEqualTo(z);
    }

    @Test
    public void testFormulas() throws ParserException {
        final Formula f1 = this.p.parse("a & (b | ~c)");
        final Formula f2 = this.p.parse("x & (b | ~c)");
        final Formula f3 = this.p.parse("x & (y | ~c)");
        assertThat(f1.transform(this.ex1)).isEqualTo(f1);
        assertThat(f1.transform(this.ex2)).isEqualTo(f1);
        assertThat(f1.transform(this.ex3)).isEqualTo(f1);
        assertThat(f1.transform(this.uni1)).isEqualTo(f1);
        assertThat(f1.transform(this.uni2)).isEqualTo(f1);
        assertThat(f1.transform(this.uni3)).isEqualTo(f1);
        assertThat(f2.transform(this.ex1)).isEqualTo(f2);
        assertThat(f2.transform(this.ex2)).isEqualTo(this.p.parse("b | ~c"));
        assertThat(f2.transform(this.ex3)).isEqualTo(this.p.parse("b | ~c"));
        assertThat(f2.transform(this.uni1)).isEqualTo(f2);
        assertThat(f2.transform(this.uni2)).isEqualTo(this.f.falsum());
        assertThat(f2.transform(this.uni3)).isEqualTo(this.f.falsum());
        assertThat(f3.transform(this.ex1)).isEqualTo(f3);
        assertThat(f3.transform(this.ex2)).isEqualTo(this.p.parse("y | ~c"));
        assertThat(f3.transform(this.ex3)).isEqualTo(this.f.verum());
        assertThat(f3.transform(this.uni1)).isEqualTo(f3);
        assertThat(f3.transform(this.uni2)).isEqualTo(this.f.falsum());
        assertThat(f3.transform(this.uni3)).isEqualTo(this.f.falsum());
    }

}
