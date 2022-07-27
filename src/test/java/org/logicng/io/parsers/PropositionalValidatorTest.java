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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

/**
 * Unit Tests for the class {@link PropositionalValidator}.
 * @version 2.0.0
 * @since 1.0
 */
public class PropositionalValidatorTest extends TestWithExampleFormulas {

    @Test
    public void testExceptions() throws ParserException {
        final PropositionalValidator parser = new PropositionalValidator();
        assertThat(parser.validate("")).isTrue();
        final String s = null;
        assertThat(parser.validate(s)).isTrue();
    }

    @Test
    public void testParseConstants() throws ParserException {
        final PropositionalValidator parser = new PropositionalValidator();
        assertThat(parser.validate("$true")).isTrue();
        assertThat(parser.validate("$false")).isTrue();
    }

    @Test
    public void testParseLiterals() throws ParserException {
        final PropositionalValidator parser = new PropositionalValidator();
        assertThat(parser.validate("A")).isTrue();
        assertThat(parser.validate("a")).isTrue();
        assertThat(parser.validate("a1")).isTrue();
        assertThat(parser.validate("aA_Bb_Cc_12_3")).isTrue();
        assertThat(parser.validate("~A")).isTrue();
        assertThat(parser.validate("~a")).isTrue();
        assertThat(parser.validate("~a1")).isTrue();
        assertThat(parser.validate("~aA_Bb_Cc_12_3")).isTrue();
        assertThat(parser.validate("~@aA_Bb_Cc_12_3")).isTrue();
    }

    @Test
    public void testParseOperators() throws ParserException {
        final PropositionalValidator parser = new PropositionalValidator();
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
    public void testParsePrecedences() throws ParserException {
        final PropositionalValidator parser = new PropositionalValidator();
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
    public void parseInputStream() throws ParserException {
        final PropositionalValidator parser = new PropositionalValidator();
        final String string = "A & B => D | ~C";
        final InputStream stream = new ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8));
        assertThat(parser.validate(stream)).isTrue();
        assertThat(parser.validate((InputStream) null)).isTrue();
    }

    @Test
    public void parseEmptyString() throws ParserException {
        final PropositionalValidator parser = new PropositionalValidator();
        assertThat(parser.validate("")).isTrue();
        assertThat(parser.validate((String) null)).isTrue();
    }

    @Test
    public void testSkipSymbols() throws ParserException {
        final PropositionalValidator parser = new PropositionalValidator();
        assertThat(parser.validate(" ")).isTrue();
        assertThat(parser.validate("\t")).isTrue();
        assertThat(parser.validate("\n")).isTrue();
        assertThat(parser.validate("\r")).isTrue();
        assertThat(parser.validate(" \r\n\n  \t")).isTrue();
        assertThat(parser.validate("a\n&\tb")).isTrue();
        assertThat(parser.validate(" a\r=>\t\tb")).isTrue();
    }

    @Test
    public void testNumericalLiteral() throws ParserException {
        final PropositionalValidator parser = new PropositionalValidator();
        assertThat(parser.validate("12")).isTrue();
    }

    @Test
    public void testIllegalVariable1() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("$$%")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalVariable3() {
        assertThatThrownBy(() -> new PropositionalValidator().validate(";;23")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalVariable4() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("{0}")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator1() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("A + B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator2() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("A &")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator3() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("A /")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator4() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("-A")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator5() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("A * B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalBrackets1() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("(A & B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula1() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("((A & B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula2() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("(A & (C & D) B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula3() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("A | A + (C | B + C)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula4() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("A | A & (C | B & C")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula5() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("A & ~B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula6() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("12)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula7() {
        assertThatThrownBy(() -> new PropositionalValidator().validate("ab@cd)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula8() {
        final String string = "A & B => D | ~";
        final InputStream stream = new ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8));
        assertThatThrownBy(() -> new PropositionalValidator().validate(stream)).isInstanceOf(ParserException.class);

    }

    @Test
    public void testIllegalFormula9() {
        final String string = "#";
        final InputStream stream = new ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8));
        assertThatThrownBy(() -> new PropositionalValidator().validate(stream)).isInstanceOf(ParserException.class);
    }

    @Test
    public void testToStrings() {
        assertThat(new PropositionalValidator().toString()).isEqualTo("PropositionalValidator");
    }
}
