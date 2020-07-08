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
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

/**
 * Unit Tests for the class {@link PseudoBooleanParser}.
 * @version 2.0.0
 * @since 1.0
 */
public class PseudoBooleanParserTest extends TestWithExampleFormulas {

    @Test
    public void testExceptions() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(this.f);
        assertThat(parser.parse("")).isEqualTo(this.f.verum());
        assertThat(parser.parse((String) null)).isEqualTo(this.f.verum());
    }

    @Test
    public void testParseConstants() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(this.f);
        assertThat(parser.parse("$true")).isEqualTo(this.f.verum());
        assertThat(parser.parse("$false")).isEqualTo(this.f.falsum());
    }

    @Test
    public void testParseLiterals() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(this.f);
        assertThat(parser.parse("A")).isEqualTo(this.f.variable("A"));
        assertThat(parser.parse("a")).isEqualTo(this.f.variable("a"));
        assertThat(parser.parse("a1")).isEqualTo(this.f.variable("a1"));
        assertThat(parser.parse("aA_Bb_Cc_12_3")).isEqualTo(this.f.variable("aA_Bb_Cc_12_3"));
        assertThat(parser.parse("~A")).isEqualTo(this.f.literal("A", false));
        assertThat(parser.parse("~a")).isEqualTo(this.f.literal("a", false));
        assertThat(parser.parse("~aA_Bb_Cc_12_3")).isEqualTo(this.f.literal("aA_Bb_Cc_12_3", false));
    }

    @Test
    public void testParseOperators() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(this.f);
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
    public void testParseMultiplication() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(this.f);
        assertThat(parser.parse("13 * abc = 4")).isEqualTo(this.f.pbc(CType.EQ, 4, new Literal[]{this.f.variable("abc")}, new int[]{13}));
        assertThat(parser.parse("-13 * a = 4")).isEqualTo(this.f.pbc(CType.EQ, 4, new Literal[]{this.f.variable("a")}, new int[]{-13}));
        assertThat(parser.parse("13 * ~abc = -442")).isEqualTo(this.f.pbc(CType.EQ, -442, new Literal[]{this.f.literal("abc", false)}, new int[]{13}));
        assertThat(parser.parse("-13 * ~a = -442")).isEqualTo(this.f.pbc(CType.EQ, -442, new Literal[]{this.f.literal("a", false)}, new int[]{-13}));
        assertThat(parser.parse("13 * abc = 4")).isEqualTo(this.f.pbc(CType.EQ, 4, new Literal[]{this.f.variable("abc")}, new int[]{13}));
        assertThat(parser.parse("13 * abc > 4")).isEqualTo(this.f.pbc(CType.GT, 4, new Literal[]{this.f.variable("abc")}, new int[]{13}));
        assertThat(parser.parse("13 * abc >= 4")).isEqualTo(this.f.pbc(CType.GE, 4, new Literal[]{this.f.variable("abc")}, new int[]{13}));
        assertThat(parser.parse("13 * abc < 4")).isEqualTo(this.f.pbc(CType.LT, 4, new Literal[]{this.f.variable("abc")}, new int[]{13}));
        assertThat(parser.parse("13 * abc <= 4")).isEqualTo(this.f.pbc(CType.LE, 4, new Literal[]{this.f.variable("abc")}, new int[]{13}));
    }

    @Test
    public void testParseAddition() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(this.f);
        assertThat(parser.parse("4 * c + -4 * ~d < -4")).isEqualTo(this.f.pbc(CType.LT, -4, new Literal[]{this.f.variable("c"), this.f.literal("d", false)}, new int[]{4, -4}));
        assertThat(parser.parse("5 * c + -5 * ~c >= -5")).isEqualTo(this.f.pbc(CType.GE, -5, new Literal[]{this.f.variable("c"), this.f.literal("c", false)}, new int[]{5, -5}));
        assertThat(parser.parse("6 * a + -6 * ~b + 12 * ~c > -6")).isEqualTo(this.f.pbc(CType.GT, -6, new Literal[]{this.f.variable("a"), this.f.literal("b", false), this.f.literal("c", false)}, new int[]{6, -6, 12}));
        assertThat(parser.parse("c + -4 * ~d < -4")).isEqualTo(this.f.pbc(CType.LT, -4, new Literal[]{this.f.variable("c"), this.f.literal("d", false)}, new int[]{1, -4}));
        assertThat(parser.parse("5 * c + ~c >= -5")).isEqualTo(this.f.pbc(CType.GE, -5, new Literal[]{this.f.variable("c"), this.f.literal("c", false)}, new int[]{5, 1}));
        assertThat(parser.parse("c + d >= -5")).isEqualTo(this.f.pbc(CType.GE, -5, new Literal[]{this.f.variable("c"), this.f.literal("d", true)}, new int[]{1, 1}));
        assertThat(parser.parse("~c + ~d >= -5")).isEqualTo(this.f.pbc(CType.GE, -5, new Literal[]{this.f.literal("c", false), this.f.literal("d", false)}, new int[]{1, 1}));
        assertThat(parser.parse("~c = -5")).isEqualTo(this.f.pbc(CType.EQ, -5, new Literal[]{this.f.literal("c", false)}, new int[]{1}));
        assertThat(parser.parse("~(c = -5)")).isEqualTo(this.f.not(this.f.pbc(CType.EQ, -5, new Literal[]{this.f.literal("c", true)}, new int[]{1})));
    }

    @Test
    public void testCombination() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(this.f);
        final Formula pbc = this.f.pbc(CType.GT, -6, new Literal[]{this.f.variable("a"), this.f.literal("b", false), this.f.literal("c", false)}, new int[]{6, -6, 12});
        assertThat(parser.parse("(x => y & z) & (6 * a + -6 * ~b + 12 * ~c > -6)")).isEqualTo(this.f.and(this.f.implication(this.f.variable("x"), this.f.and(this.f.variable("y"), this.f.variable("z"))), pbc));
        assertThat(parser.parse("~(6 * a - 6 * ~b - -12 * ~c > -6)")).isEqualTo(this.f.not(pbc));
    }

    @Test
    public void testParsePrecedences() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(this.f);
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
    public void parseEmptyString() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(this.f);
        assertThat(parser.parse("")).isEqualTo(this.f.verum());
    }

    @Test
    public void testSkipSymbols() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(this.f);
        assertThat(parser.parse(" ")).isEqualTo(this.f.verum());
        assertThat(parser.parse("\t")).isEqualTo(this.f.verum());
        assertThat(parser.parse("\n")).isEqualTo(this.f.verum());
        assertThat(parser.parse("\r")).isEqualTo(this.f.verum());
        assertThat(parser.parse(" \r\n\n  \t")).isEqualTo(this.f.verum());
        assertThat(parser.parse("a\n&\tb")).isEqualTo(this.AND1);
        assertThat(parser.parse(" a\r=>\t\tb")).isEqualTo(this.IMP1);
        assertThat(parser.parse(" 2\n*a\r+\n\n-4*\tb    +3*x=2")).isEqualTo(this.PBC1);
    }

    @Test
    public void testNumberLiterals() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser parser = new PseudoBooleanParser(f);
        assertThat(parser.parse("12 & A")).isEqualTo(f.and(f.variable("12"), f.variable("A")));
        assertThat(parser.parse("~12 & A")).isEqualTo(f.and(f.literal("12", false), f.variable("A")));
        assertThat(parser.parse("12 * 12 + 13 * A + 10 * B <= 25")).isEqualTo(f.pbc(CType.LE, 25, new Literal[]{f.variable("12"), f.variable("A"), f.variable("B")}, new int[]{12, 13, 10}));
        assertThat(parser.parse("-12 * ~12 + 13 * A + 10 * B <= 25")).isEqualTo(f.pbc(CType.LE, 25, new Literal[]{f.literal("12", false), f.variable("A"), f.variable("B")}, new int[]{-12, 13, 10}));
    }

    @Test
    public void testFormulaFactoryParser() throws ParserException {
        assertThat(this.f.parse("a & b")).isEqualTo(this.f.and(this.f.variable("a"), this.f.variable("b")));
        assertThat(this.f.parse("2*a + -4*b + 3*x = 2")).isEqualTo(this.PBC1);
    }

    @Test
    public void testIllegalVariable1() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("$$%")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalVariable3() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse(";;23")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalVariable4() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("{0}")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator1() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("A + B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator2() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("A &")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator3() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("A /")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator4() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("-A")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator5() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("A * B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalBrackets1() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("(A & B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula1() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("((A & B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula2() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("(A & (C & D) B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula3() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("A | A + (C | B + C)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula4() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("A | A & (C | B & C")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula5() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("A & ~B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula6() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("12)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula7() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("ab@cd)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalSkipPosition() {
        assertThatThrownBy(() -> new PseudoBooleanParser(this.f).parse("- 1*x <= 3")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testToStrings() {
        assertThat(new PseudoBooleanLexer(null).toString()).isEqualTo("PseudoBooleanLexer");
        assertThat(new PseudoBooleanParser(this.f).toString()).isEqualTo("PseudoBooleanParser");
    }
}
