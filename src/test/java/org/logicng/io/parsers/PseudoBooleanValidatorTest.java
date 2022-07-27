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

package org.logicng.io.parsers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.CType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

public class PseudoBooleanValidatorTest extends TestWithExampleFormulas {

    @Test
    public void testExceptions() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        assertThat(parser.validate("")).isTrue();
        assertThat(parser.validate((String) null)).isTrue();
    }

    @Test
    public void testParseConstants() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        assertThat(parser.validate("$true")).isTrue();
        assertThat(parser.validate("$false")).isTrue();
    }

    @Test
    public void testParseLiterals() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        assertThat(parser.validate("A")).isTrue();
        assertThat(parser.validate("a")).isTrue();
        assertThat(parser.validate("a1")).isTrue();
        assertThat(parser.validate("aA_Bb_Cc_12_3")).isTrue();
        assertThat(parser.validate("~A")).isTrue();
        assertThat(parser.validate("~a")).isTrue();
        assertThat(parser.validate("~aA_Bb_Cc_12_3")).isTrue();
    }

    @Test
    public void testParseOperators() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        assertThat(parser.validate("~a")).isTrue();
        assertThat(parser.validate("~Var")).isTrue();
        assertThat(parser.validate("a & b")).isTrue();
        assertThat(parser.validate("~a & ~b")).isTrue();
        assertThat(parser.validate("~a & b & ~c & d")).isTrue();
        assertThat(parser.validate("a | b")).isTrue();
        assertThat(parser.validate("~a | ~b")).isTrue();
        assertThat(parser.validate("~a | b | ~c | d")).isTrue();
        assertThat(parser.validate("a => b")).isTrue();
        assertThat(parser.validate("~a => ~b")).isTrue();
        assertThat(parser.validate("a <=> b")).isTrue();
        assertThat(parser.validate("~a <=> ~b")).isTrue();
    }

    @Test
    public void testParseMultiplication() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        assertThat(parser.validate("13 * abc = 4")).isTrue();
        assertThat(parser.validate("-13 * a = 4")).isTrue();
        assertThat(parser.validate("13 * ~abc = -442")).isTrue();
        assertThat(parser.validate("-13 * ~a = -442")).isTrue();
        assertThat(parser.validate("13 * abc = 4")).isTrue();
        assertThat(parser.validate("13 * abc > 4")).isTrue();
        assertThat(parser.validate("13 * abc >= 4")).isTrue();
        assertThat(parser.validate("13 * abc < 4")).isTrue();
        assertThat(parser.validate("13 * abc <= 4")).isTrue();
    }

    @Test
    public void testParseAddition() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        assertThat(parser.validate("4 * c + -4 * ~d < -4")).isTrue();
        assertThat(parser.validate("5 * c + -5 * ~c >= -5")).isTrue();
        assertThat(parser.validate("6 * a + -6 * ~b + 12 * ~c > -6")).isTrue();
        assertThat(parser.validate("c + -4 * ~d < -4")).isTrue();
        assertThat(parser.validate("5 * c + ~c >= -5")).isTrue();
        assertThat(parser.validate("c + d >= -5")).isTrue();
        assertThat(parser.validate("~c + ~d >= -5")).isTrue();
        assertThat(parser.validate("~c = -5")).isTrue();
        assertThat(parser.validate("~(c = -5)")).isTrue();
    }

    @Test
    public void testCombination() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        final Formula pbc = this.f.pbc(CType.GT, -6, new Literal[]{this.f.variable("a"), this.f.literal("b", false), this.f.literal("c", false)}, new int[]{6, -6, 12});
        assertThat(parser.validate("(x => y & z) & (6 * a + -6 * ~b + 12 * ~c > -6)")).isTrue();
        assertThat(parser.validate("~(6 * a - 6 * ~b - -12 * ~c > -6)")).isTrue();
    }

    @Test
    public void testParsePrecedences() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        assertThat(parser.validate("x | y & z")).isTrue();
        assertThat(parser.validate("x & y | z")).isTrue();
        assertThat(parser.validate("x => y & z")).isTrue();
        assertThat(parser.validate("x & y => z")).isTrue();
        assertThat(parser.validate("x <=> y & z")).isTrue();
        assertThat(parser.validate("x & y <=> z")).isTrue();
        assertThat(parser.validate("x => y | z")).isTrue();
        assertThat(parser.validate("x | y => z")).isTrue();
        assertThat(parser.validate("x <=> y | z")).isTrue();
        assertThat(parser.validate("x | y <=> z")).isTrue();
        assertThat(parser.validate("x => y => z")).isTrue();
        assertThat(parser.validate("x <=> y <=> z")).isTrue();
        assertThat(parser.validate("(x | y) & z")).isTrue();
        assertThat(parser.validate("x & (y | z)")).isTrue();
        assertThat(parser.validate("(x => y) & z")).isTrue();
        assertThat(parser.validate("x & (y => z)")).isTrue();
        assertThat(parser.validate("(x => y) | z")).isTrue();
        assertThat(parser.validate("x | (y => z)")).isTrue();
        assertThat(parser.validate("(x <=> y) & z")).isTrue();
        assertThat(parser.validate("x & (y <=> z)")).isTrue();
        assertThat(parser.validate("(x <=> y) | z")).isTrue();
        assertThat(parser.validate("x | (y <=> z)")).isTrue();
        assertThat(parser.validate("x => y <=> z")).isTrue();
        assertThat(parser.validate("x => (y <=> z)")).isTrue();
    }

    @Test
    public void parseEmptyString() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        assertThat(parser.validate("")).isTrue();
    }

    @Test
    public void testSkipSymbols() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        assertThat(parser.validate(" ")).isTrue();
        assertThat(parser.validate("\t")).isTrue();
        assertThat(parser.validate("\n")).isTrue();
        assertThat(parser.validate("\r")).isTrue();
        assertThat(parser.validate(" \r\n\n  \t")).isTrue();
        assertThat(parser.validate("a\n&\tb")).isTrue();
        assertThat(parser.validate(" a\r=>\t\tb")).isTrue();
        assertThat(parser.validate(" 2\n*a\r+\n\n-4*\tb    +3*x=2")).isTrue();
    }

    @Test
    public void testNumberLiterals() throws ParserException {
        final PseudoBooleanValidator parser = new PseudoBooleanValidator();
        assertThat(parser.validate("12 & A")).isTrue();
        assertThat(parser.validate("~12 & A")).isTrue();
        assertThat(parser.validate("12 * 12 + 13 * A + 10 * B <= 25")).isTrue();
        assertThat(parser.validate("-12 * ~12 + 13 * A + 10 * B <= 25")).isTrue();
    }

    @Test
    public void testIllegalVariable1() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("$$%")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalVariable3() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate(";;23")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalVariable4() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("{0}")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator1() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("A + B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator2() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("A &")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator3() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("A /")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator4() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("-A")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator5() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("A * B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalBrackets1() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("(A & B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula1() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("((A & B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula2() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("(A & (C & D) B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula3() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("A | A + (C | B + C)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula4() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("A | A & (C | B & C")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula5() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("A & ~B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula6() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("12)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula7() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("ab@cd)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalSkipPosition() {
        assertThatThrownBy(() -> new PseudoBooleanValidator().validate("- 1*x <= 3")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testToStrings() {
        assertThat(new PseudoBooleanValidator().toString()).isEqualTo("PseudoBooleanValidator");
    }
}
