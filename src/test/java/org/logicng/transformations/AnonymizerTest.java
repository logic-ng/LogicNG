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

package org.logicng.transformations;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;

/**
 * Unit tests for {@link Anonymizer}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class AnonymizerTest {

    @Test
    public void testSimpleFormulasDefault() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Anonymizer anonymizer = new Anonymizer();
        assertThat(p.parse("$true").transform(anonymizer)).isEqualTo(p.parse("$true"));
        assertThat(p.parse("$false").transform(anonymizer)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A").transform(anonymizer)).isEqualTo(p.parse("v0"));
        assertThat(p.parse("~A").transform(anonymizer)).isEqualTo(p.parse("~v0"));
        assertThat(p.parse("A => ~B").transform(anonymizer)).isEqualTo(p.parse("v0 => ~v1"));
        assertThat(p.parse("A <=> ~B").transform(anonymizer)).isEqualTo(p.parse("v0 <=> ~v1"));
        assertThat(p.parse("A | B | ~D | C").transform(anonymizer)).isEqualTo(p.parse("v0 | v1 | ~v3 | v2"));
        assertThat(p.parse("A & B & C & ~D").transform(anonymizer)).isEqualTo(p.parse("v0 & v1 & v2 & ~v3"));
    }

    @Test
    public void testSimpleFormulasOwnPrefix() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Anonymizer anonymizer = new Anonymizer("var");
        assertThat(p.parse("$true").transform(anonymizer)).isEqualTo(p.parse("$true"));
        assertThat(p.parse("$false").transform(anonymizer)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A").transform(anonymizer)).isEqualTo(p.parse("var0"));
        assertThat(p.parse("~A").transform(anonymizer)).isEqualTo(p.parse("~var0"));
        assertThat(p.parse("A => ~B").transform(anonymizer)).isEqualTo(p.parse("var0 => ~var1"));
        assertThat(p.parse("A <=> ~B").transform(anonymizer)).isEqualTo(p.parse("var0 <=> ~var1"));
        assertThat(p.parse("A <=> ~B").transform(anonymizer)).isEqualTo(p.parse("var0 <=> ~var1"));
        assertThat(p.parse("A | B | ~D | C").transform(anonymizer)).isEqualTo(p.parse("var0 | var1 | ~var3 | var2"));
        assertThat(p.parse("A & B & C & ~D").transform(anonymizer)).isEqualTo(p.parse("var0 & var1 & var2 & ~var3"));
    }

    @Test
    public void testSimpleFormulasOwnPrefixAndCounter() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Anonymizer anonymizer = new Anonymizer("var", 10);
        assertThat(p.parse("$true").transform(anonymizer)).isEqualTo(p.parse("$true"));
        assertThat(p.parse("$false").transform(anonymizer)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A").transform(anonymizer)).isEqualTo(p.parse("var10"));
        assertThat(p.parse("~A").transform(anonymizer)).isEqualTo(p.parse("~var10"));
        assertThat(p.parse("A => ~B").transform(anonymizer)).isEqualTo(p.parse("var10 => ~var11"));
        assertThat(p.parse("A <=> ~B").transform(anonymizer)).isEqualTo(p.parse("var10 <=> ~var11"));
        assertThat(p.parse("A | B | ~D | C").transform(anonymizer)).isEqualTo(p.parse("var10 | var11 | ~var13 | var12"));
        assertThat(p.parse("A & B & C & ~D").transform(anonymizer)).isEqualTo(p.parse("var10 & var11 & var12 & ~var13"));
    }

    @Test
    public void testSimpleFormulasOwnPrefixAndCounterWithoutCache() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Anonymizer anonymizer = new Anonymizer("var", 10);
        assertThat(p.parse("$true").transform(anonymizer, false)).isEqualTo(p.parse("$true"));
        assertThat(p.parse("$false").transform(anonymizer, false)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("A").transform(anonymizer, false)).isEqualTo(p.parse("var10"));
        assertThat(p.parse("~A").transform(anonymizer, false)).isEqualTo(p.parse("~var10"));
        assertThat(p.parse("A => ~B").transform(anonymizer, false)).isEqualTo(p.parse("var10 => ~var11"));
        assertThat(p.parse("A <=> ~B").transform(anonymizer, false)).isEqualTo(p.parse("var10 <=> ~var11"));
        assertThat(p.parse("A | B | ~D | C").transform(anonymizer, false)).isEqualTo(p.parse("var10 | var11 | ~var13 | var12"));
        assertThat(p.parse("A & B & C & ~D").transform(anonymizer, false)).isEqualTo(p.parse("var10 & var11 & var12 & ~var13"));
    }
}
