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
 * Unit Tests for the class {@link PropositionalParser}.
 * @version 2.0.0
 * @since 1.0
 */
public class PropositionalParserTest extends TestWithExampleFormulas {

    @Test
    public void testExceptions() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        assertThat(parser.parse("")).isEqualTo(this.f.verum());
        final String s = null;
        assertThat(parser.parse(s)).isEqualTo(this.f.verum());
    }

    @Test
    public void testParseConstants() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        assertThat(parser.parse("$true")).isEqualTo(this.f.verum());
        assertThat(parser.parse("$false")).isEqualTo(this.f.falsum());
    }

    @Test
    public void testParseLiterals() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        assertThat(parser.parse("A")).isEqualTo(this.f.variable("A"));
        assertThat(parser.parse("a")).isEqualTo(this.f.variable("a"));
        assertThat(parser.parse("a1")).isEqualTo(this.f.variable("a1"));
        assertThat(parser.parse("aA_Bb_Cc_12_3")).isEqualTo(this.f.variable("aA_Bb_Cc_12_3"));
        assertThat(parser.parse("~A")).isEqualTo(this.f.literal("A", false));
        assertThat(parser.parse("~a")).isEqualTo(this.f.literal("a", false));
        assertThat(parser.parse("~a1")).isEqualTo(this.f.literal("a1", false));
        assertThat(parser.parse("~aA_Bb_Cc_12_3")).isEqualTo(this.f.literal("aA_Bb_Cc_12_3", false));
        assertThat(parser.parse("~@aA_Bb_Cc_12_3")).isEqualTo(this.f.literal("@aA_Bb_Cc_12_3", false));
    }

    @Test
    public void testParseOperators() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        assertThat(parser.parse("~a")).isEqualTo(this.f.not(this.f.variable("a")));
        assertThat(parser.parse("~Var")).isEqualTo(this.f.not(this.f.variable("Var")));
        assertThat(parser.parse("a & b")).isEqualTo(this.f.and(this.f.variable("a"), this.f.variable("b")));
        assertThat(parser.parse("~a & ~b")).isEqualTo(this.f.and(this.f.literal("a", false), this.f.literal("b", false)));
        assertThat(parser.parse("~a & b & ~c & d")).isEqualTo(this.f.and(this.f.literal("a", false), this.f.variable("b"), this.f.literal("c", false), this.f.variable("d")));
        assertThat(parser.parse("a | b")).isEqualTo(this.f.or(this.f.variable("a"), this.f.variable("b")));
        assertThat(parser.parse("~a | ~b")).isEqualTo(this.f.or(this.f.literal("a", false), this.f.literal("b", false)));
        assertThat(parser.parse("~a | b | ~c | d")).isEqualTo(this.f.or(this.f.literal("a", false), this.f.variable("b"), this.f.literal("c", false), this.f.variable("d")));
        assertThat(parser.parse("a => b")).isEqualTo(this.f.implication(this.f.variable("a"), this.f.variable("b")));
        assertThat(parser.parse("~a => ~b")).isEqualTo(this.f.implication(this.f.literal("a", false), this.f.literal("b", false)));
        assertThat(parser.parse("a <=> b")).isEqualTo(this.f.equivalence(this.f.variable("a"), this.f.variable("b")));
        assertThat(parser.parse("~a <=> ~b")).isEqualTo(this.f.equivalence(this.f.literal("a", false), this.f.literal("b", false)));
    }

    @Test
    public void testParsePrecedences() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        assertThat(parser.parse("x | y & z")).isEqualTo(this.f.or(this.f.variable("x"), this.f.and(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("x & y | z")).isEqualTo(this.f.or(this.f.and(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x => y & z")).isEqualTo(this.f.implication(this.f.variable("x"), this.f.and(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("x & y => z")).isEqualTo(this.f.implication(this.f.and(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x <=> y & z")).isEqualTo(this.f.equivalence(this.f.variable("x"), this.f.and(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("x & y <=> z")).isEqualTo(this.f.equivalence(this.f.and(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x => y | z")).isEqualTo(this.f.implication(this.f.variable("x"), this.f.or(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("x | y => z")).isEqualTo(this.f.implication(this.f.or(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x <=> y | z")).isEqualTo(this.f.equivalence(this.f.variable("x"), this.f.or(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("x | y <=> z")).isEqualTo(this.f.equivalence(this.f.or(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x => y => z")).isEqualTo(this.f.implication(this.f.variable("x"), this.f.implication(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("x <=> y <=> z")).isEqualTo(this.f.equivalence(this.f.variable("x"), this.f.equivalence(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("(x | y) & z")).isEqualTo(this.f.and(this.f.or(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x & (y | z)")).isEqualTo(this.f.and(this.f.variable("x"), this.f.or(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("(x => y) & z")).isEqualTo(this.f.and(this.f.implication(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x & (y => z)")).isEqualTo(this.f.and(this.f.variable("x"), this.f.implication(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("(x => y) | z")).isEqualTo(this.f.or(this.f.implication(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x | (y => z)")).isEqualTo(this.f.or(this.f.variable("x"), this.f.implication(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("(x <=> y) & z")).isEqualTo(this.f.and(this.f.equivalence(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x & (y <=> z)")).isEqualTo(this.f.and(this.f.variable("x"), this.f.equivalence(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("(x <=> y) | z")).isEqualTo(this.f.or(this.f.equivalence(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x | (y <=> z)")).isEqualTo(this.f.or(this.f.variable("x"), this.f.equivalence(this.f.variable("y"), this.f.variable("z"))));
        assertThat(parser.parse("x => y <=> z")).isEqualTo(this.f.equivalence(this.f.implication(this.f.variable("x"), this.f.variable("y")), this.f.variable("z")));
        assertThat(parser.parse("x => (y <=> z)")).isEqualTo(this.f.implication(this.f.variable("x"), this.f.equivalence(this.f.variable("y"), this.f.variable("z"))));
    }

    @Test
    public void parseInputStream() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        final String string = "A & B => D | ~C";
        final InputStream stream = new ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8));
        assertThat(parser.parse(stream)).isEqualTo(parser.parse(string));
        assertThat(parser.parse((InputStream) null)).isEqualTo(this.f.verum());
    }

    @Test
    public void parseEmptyString() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        assertThat(parser.parse("")).isEqualTo(this.f.verum());
        assertThat(parser.parse((String) null)).isEqualTo(this.f.verum());
    }

    @Test
    public void testFormulaFactory() {
        final PropositionalParser parser = new PropositionalParser(this.f);
        assertThat(parser.factory()).isEqualTo(this.f);
    }

    @Test
    public void testSkipSymbols() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        assertThat(parser.parse(" ")).isEqualTo(this.f.verum());
        assertThat(parser.parse("\t")).isEqualTo(this.f.verum());
        assertThat(parser.parse("\n")).isEqualTo(this.f.verum());
        assertThat(parser.parse("\r")).isEqualTo(this.f.verum());
        assertThat(parser.parse(" \r\n\n  \t")).isEqualTo(this.f.verum());
        assertThat(parser.parse("a\n&\tb")).isEqualTo(this.AND1);
        assertThat(parser.parse(" a\r=>\t\tb")).isEqualTo(this.IMP1);
    }

    @Test
    public void testNumericalLiteral() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        assertThat(parser.parse("12")).isEqualTo(this.f.variable("12"));
    }

    @Test
    public void testIllegalVariable1() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("$$%")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalVariable3() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse(";;23")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalVariable4() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("{0}")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator1() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("A + B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator2() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("A &")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator3() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("A /")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator4() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("-A")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator5() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("A * B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalBrackets1() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("(A & B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula1() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("((A & B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula2() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("(A & (C & D) B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula3() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("A | A + (C | B + C)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula4() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("A | A & (C | B & C")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula5() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("A & ~B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula6() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("12)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula7() {
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse("ab@cd)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula8() {
        final String string = "A & B => D | ~";
        final InputStream stream = new ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8));
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse(stream)).isInstanceOf(ParserException.class);

    }

    @Test
    public void testIllegalFormula9() {
        final String string = "#";
        final InputStream stream = new ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8));
        assertThatThrownBy(() -> new PropositionalParser(this.f).parse(stream)).isInstanceOf(ParserException.class);
    }

    @Test
    public void testToStrings() {
        assertThat(new PropositionalLexer(null).toString()).isEqualTo("PropositionalLexer");
        assertThat(new PropositionalParser(this.f).toString()).isEqualTo("PropositionalParser");
    }
}
