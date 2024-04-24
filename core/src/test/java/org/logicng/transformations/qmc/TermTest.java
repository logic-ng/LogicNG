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

package org.logicng.transformations.qmc;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Unit tests for {@link Term}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class TermTest {

    @Test
    public void testUnite() {
        final Tristate[] bits1 = new Tristate[]{TRUE, TRUE, TRUE, TRUE};
        final Tristate[] bits2 = new Tristate[]{TRUE, FALSE, TRUE, TRUE};
        final Tristate[] bits3 = new Tristate[]{TRUE, TRUE, FALSE, TRUE};
        final Tristate[] bits4 = new Tristate[]{FALSE, FALSE, TRUE, TRUE};
        final Tristate[] bits5 = new Tristate[]{TRUE, FALSE, FALSE, UNDEF};
        final Tristate[] bits6 = new Tristate[]{TRUE, FALSE, TRUE, UNDEF};
        final Tristate[] bits7 = new Tristate[]{TRUE, FALSE, TRUE};

        final FormulaFactory f = new FormulaFactory();
        final List<Formula> formulas1 = Collections.singletonList(f.variable("f1"));
        final List<Formula> formulas2 = Collections.singletonList(f.variable("f2"));
        final List<Formula> formulas3 = Collections.singletonList(f.variable("f3"));
        final List<Formula> formulas4 = Collections.singletonList(f.variable("f4"));
        final List<Formula> formulas5 = Collections.singletonList(f.variable("f5"));
        final List<Formula> formulas6 = Collections.singletonList(f.variable("f6"));
        final List<Formula> formulas7 = Collections.singletonList(f.variable("f7"));

        final Term term1 = new Term(bits1, formulas1);
        final Term term2 = new Term(bits2, formulas2);
        final Term term3 = new Term(bits3, formulas3);
        final Term term4 = new Term(bits4, formulas4);
        final Term term5 = new Term(bits5, formulas5);
        final Term term6 = new Term(bits6, formulas6);
        final Term term7 = new Term(bits7, formulas7);

        assertThat(term1.combine(term2)).isEqualTo(new Term(new Tristate[]{TRUE, UNDEF, TRUE, TRUE}, Arrays.asList(f.variable("f1"), f.variable("f2"))));
        assertThat(term1.combine(term3)).isEqualTo(new Term(new Tristate[]{TRUE, TRUE, UNDEF, TRUE}, Arrays.asList(f.variable("f1"), f.variable("f3"))));
        assertThat(term2.combine(term3)).isNull();
        assertThat(term1.combine(term4)).isNull();
        assertThat(term2.combine(term4)).isEqualTo(new Term(new Tristate[]{UNDEF, FALSE, TRUE, TRUE}, Arrays.asList(f.variable("f2"), f.variable("f4"))));
        assertThat(term3.combine(term4)).isNull();
        assertThat(term5.combine(term6)).isEqualTo(new Term(new Tristate[]{TRUE, FALSE, UNDEF, UNDEF}, Arrays.asList(f.variable("f5"), f.variable("f6"))));
        assertThat(term1.combine(term7)).isNull();
    }

    @Test
    public void testToFormula() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Tristate[] bits1 = new Tristate[]{TRUE, TRUE, TRUE, TRUE};
        final Tristate[] bits2 = new Tristate[]{TRUE, FALSE, TRUE, TRUE};
        final Tristate[] bits3 = new Tristate[]{TRUE, TRUE, FALSE, TRUE};
        final Tristate[] bits4 = new Tristate[]{FALSE, FALSE, TRUE, TRUE};
        final Tristate[] bits5 = new Tristate[]{TRUE, FALSE, FALSE, UNDEF};
        final Tristate[] bits6 = new Tristate[]{TRUE, UNDEF, TRUE, UNDEF};
        final List<Formula> formulas1 = Collections.singletonList(f.variable("f1"));
        final List<Formula> formulas2 = Collections.singletonList(f.variable("f2"));
        final List<Formula> formulas3 = Collections.singletonList(f.variable("f3"));
        final List<Formula> formulas4 = Collections.singletonList(f.variable("f4"));
        final List<Formula> formulas5 = Collections.singletonList(f.variable("f5"));
        final List<Formula> formulas6 = Collections.singletonList(f.variable("f6"));

        final Term term1 = new Term(bits1, formulas1);
        final Term term2 = new Term(bits2, formulas2);
        final Term term3 = new Term(bits3, formulas3);
        final Term term4 = new Term(bits4, formulas4);
        final Term term5 = new Term(bits5, formulas5);
        final Term term6 = new Term(bits6, formulas6);
        final List<Variable> varOrder = Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C"), f.variable("D"));

        assertThat(term1.translateToFormula(varOrder)).isEqualTo(p.parse("A & B & C & D"));
        assertThat(term2.translateToFormula(varOrder)).isEqualTo(p.parse("A & ~B & C & D"));
        assertThat(term3.translateToFormula(varOrder)).isEqualTo(p.parse("A & B & ~C & D"));
        assertThat(term4.translateToFormula(varOrder)).isEqualTo(p.parse("~A & ~B & C & D"));
        assertThat(term5.translateToFormula(varOrder)).isEqualTo(p.parse("A & ~B & ~C"));
        assertThat(term6.translateToFormula(varOrder)).isEqualTo(p.parse("A & C"));
        assertThat(term1.combine(term1)).isNull();
    }

    @Test
    public void testToString() {
        final FormulaFactory f = new FormulaFactory();
        final Tristate[] bits1 = new Tristate[]{TRUE, TRUE, TRUE, TRUE};
        final Tristate[] bits2 = new Tristate[]{TRUE, FALSE, TRUE, TRUE};
        final List<Formula> formulas1 = Collections.singletonList(f.variable("f1"));
        final List<Formula> formulas2 = Collections.singletonList(f.variable("f2"));
        final Term term1 = new Term(bits1, formulas1);
        final Term term2 = new Term(bits2, formulas2);
        assertThat(term1.toString()).isEqualTo("Term{bits=[TRUE, TRUE, TRUE, TRUE], minterms=[f1], termClass=4}");
        assertThat(term2.toString()).isEqualTo("Term{bits=[TRUE, FALSE, TRUE, TRUE], minterms=[f2], termClass=3}");
    }

}
