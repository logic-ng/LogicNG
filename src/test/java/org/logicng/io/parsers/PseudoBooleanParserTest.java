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
import org.logicng.formulas.CType;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

/**
 * Unit Tests for the class {@link PseudoBooleanParser}.
 * @version 2.0.0
 * @since 1.0
 */
public class PseudoBooleanParserTest {

    @Test
    public void testExceptions() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
        assertThat(parser.parse("")).isEqualTo(F.f.verum());
        assertThat(parser.parse((String) null)).isEqualTo(F.f.verum());
    }

    @Test
    public void testParseConstants() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
        assertThat(parser.parse("$true")).isEqualTo(F.f.verum());
        assertThat(parser.parse("$false")).isEqualTo(F.f.falsum());
    }

    @Test
    public void testParseLiterals() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
        assertThat(parser.parse("A")).isEqualTo(F.f.variable("A"));
        assertThat(parser.parse("a")).isEqualTo(F.f.variable("a"));
        assertThat(parser.parse("a1")).isEqualTo(F.f.variable("a1"));
        assertThat(parser.parse("aA_Bb_Cc_12_3")).isEqualTo(F.f.variable("aA_Bb_Cc_12_3"));
        assertThat(parser.parse("~A")).isEqualTo(F.f.literal("A", false));
        assertThat(parser.parse("~a")).isEqualTo(F.f.literal("a", false));
        assertThat(parser.parse("~aA_Bb_Cc_12_3")).isEqualTo(F.f.literal("aA_Bb_Cc_12_3", false));
    }

    @Test
    public void testParseOperators() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
        assertThat(parser.parse("~a")).isEqualTo(F.f.not(F.f.variable("a")));
        assertThat(parser.parse("~Var")).isEqualTo(F.f.not(F.f.variable("Var")));
        assertThat(parser.parse("a & b")).isEqualTo(F.f.and(F.f.variable("a"), F.f.variable("b")));
        assertThat(parser.parse("~a & ~b")).isEqualTo(F.f.and(F.f.literal("a", false), F.f.literal("b", false)));
        assertThat(parser.parse("~a & b & ~c & d")).isEqualTo(F.f.and(F.f.literal("a", false), F.f.variable("b"), F.f.literal("c", false), F.f.variable("d")));
        assertThat(parser.parse("a | b")).isEqualTo(F.f.or(F.f.variable("a"), F.f.variable("b")));
        assertThat(parser.parse("~a | ~b")).isEqualTo(F.f.or(F.f.literal("a", false), F.f.literal("b", false)));
        assertThat(parser.parse("~a | b | ~c | d")).isEqualTo(F.f.or(F.f.literal("a", false), F.f.variable("b"), F.f.literal("c", false), F.f.variable("d")));
        assertThat(parser.parse("a => b")).isEqualTo(F.f.implication(F.f.variable("a"), F.f.variable("b")));
        assertThat(parser.parse("~a => ~b")).isEqualTo(F.f.implication(F.f.literal("a", false), F.f.literal("b", false)));
        assertThat(parser.parse("a <=> b")).isEqualTo(F.f.equivalence(F.f.variable("a"), F.f.variable("b")));
        assertThat(parser.parse("~a <=> ~b")).isEqualTo(F.f.equivalence(F.f.literal("a", false), F.f.literal("b", false)));
    }

    @Test
    public void testParseMultiplication() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
        assertThat(parser.parse("13 * abc = 4")).isEqualTo(F.f.pbc(CType.EQ, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}));
        assertThat(parser.parse("-13 * a = 4")).isEqualTo(F.f.pbc(CType.EQ, 4, new Literal[]{F.f.variable("a")}, new int[]{-13}));
        assertThat(parser.parse("13 * ~abc = -442")).isEqualTo(F.f.pbc(CType.EQ, -442, new Literal[]{F.f.literal("abc", false)}, new int[]{13}));
        assertThat(parser.parse("-13 * ~a = -442")).isEqualTo(F.f.pbc(CType.EQ, -442, new Literal[]{F.f.literal("a", false)}, new int[]{-13}));
        assertThat(parser.parse("13 * abc = 4")).isEqualTo(F.f.pbc(CType.EQ, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}));
        assertThat(parser.parse("13 * abc > 4")).isEqualTo(F.f.pbc(CType.GT, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}));
        assertThat(parser.parse("13 * abc >= 4")).isEqualTo(F.f.pbc(CType.GE, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}));
        assertThat(parser.parse("13 * abc < 4")).isEqualTo(F.f.pbc(CType.LT, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}));
        assertThat(parser.parse("13 * abc <= 4")).isEqualTo(F.f.pbc(CType.LE, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}));
    }

    @Test
    public void testParseAddition() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
        assertThat(parser.parse("4 * c + -4 * ~d < -4")).isEqualTo(F.f.pbc(CType.LT, -4, new Literal[]{F.f.variable("c"), F.f.literal("d", false)}, new int[]{4, -4}));
        assertThat(parser.parse("5 * c + -5 * ~c >= -5")).isEqualTo(F.f.pbc(CType.GE, -5, new Literal[]{F.f.variable("c"), F.f.literal("c", false)}, new int[]{5, -5}));
        assertThat(parser.parse("6 * a + -6 * ~b + 12 * ~c > -6")).isEqualTo(F.f.pbc(CType.GT, -6, new Literal[]{F.f.variable("a"), F.f.literal("b", false), F.f.literal("c", false)}, new int[]{6, -6, 12}));
        assertThat(parser.parse("c + -4 * ~d < -4")).isEqualTo(F.f.pbc(CType.LT, -4, new Literal[]{F.f.variable("c"), F.f.literal("d", false)}, new int[]{1, -4}));
        assertThat(parser.parse("5 * c + ~c >= -5")).isEqualTo(F.f.pbc(CType.GE, -5, new Literal[]{F.f.variable("c"), F.f.literal("c", false)}, new int[]{5, 1}));
        assertThat(parser.parse("c + d >= -5")).isEqualTo(F.f.pbc(CType.GE, -5, new Literal[]{F.f.variable("c"), F.f.literal("d", true)}, new int[]{1, 1}));
        assertThat(parser.parse("~c + ~d >= -5")).isEqualTo(F.f.pbc(CType.GE, -5, new Literal[]{F.f.literal("c", false), F.f.literal("d", false)}, new int[]{1, 1}));
        assertThat(parser.parse("~c = -5")).isEqualTo(F.f.pbc(CType.EQ, -5, new Literal[]{F.f.literal("c", false)}, new int[]{1}));
        assertThat(parser.parse("~(c = -5)")).isEqualTo(F.f.not(F.f.pbc(CType.EQ, -5, new Literal[]{F.f.literal("c", true)}, new int[]{1})));
    }

    @Test
    public void testCombination() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
        final Formula pbc = F.f.pbc(CType.GT, -6, new Literal[]{F.f.variable("a"), F.f.literal("b", false), F.f.literal("c", false)}, new int[]{6, -6, 12});
        assertThat(parser.parse("(x => y & z) & (6 * a + -6 * ~b + 12 * ~c > -6)")).isEqualTo(F.f.and(F.f.implication(F.f.variable("x"), F.f.and(F.f.variable("y"), F.f.variable("z"))), pbc));
        assertThat(parser.parse("~(6 * a - 6 * ~b - -12 * ~c > -6)")).isEqualTo(F.f.not(pbc));
    }

    @Test
    public void testParsePrecedences() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
        assertThat(parser.parse("x | y & z")).isEqualTo(F.f.or(F.f.variable("x"), F.f.and(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("x & y | z")).isEqualTo(F.f.or(F.f.and(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x => y & z")).isEqualTo(F.f.implication(F.f.variable("x"), F.f.and(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("x & y => z")).isEqualTo(F.f.implication(F.f.and(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x <=> y & z")).isEqualTo(F.f.equivalence(F.f.variable("x"), F.f.and(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("x & y <=> z")).isEqualTo(F.f.equivalence(F.f.and(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x => y | z")).isEqualTo(F.f.implication(F.f.variable("x"), F.f.or(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("x | y => z")).isEqualTo(F.f.implication(F.f.or(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x <=> y | z")).isEqualTo(F.f.equivalence(F.f.variable("x"), F.f.or(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("x | y <=> z")).isEqualTo(F.f.equivalence(F.f.or(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x => y => z")).isEqualTo(F.f.implication(F.f.variable("x"), F.f.implication(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("x <=> y <=> z")).isEqualTo(F.f.equivalence(F.f.variable("x"), F.f.equivalence(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("(x | y) & z")).isEqualTo(F.f.and(F.f.or(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x & (y | z)")).isEqualTo(F.f.and(F.f.variable("x"), F.f.or(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("(x => y) & z")).isEqualTo(F.f.and(F.f.implication(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x & (y => z)")).isEqualTo(F.f.and(F.f.variable("x"), F.f.implication(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("(x => y) | z")).isEqualTo(F.f.or(F.f.implication(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x | (y => z)")).isEqualTo(F.f.or(F.f.variable("x"), F.f.implication(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("(x <=> y) & z")).isEqualTo(F.f.and(F.f.equivalence(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x & (y <=> z)")).isEqualTo(F.f.and(F.f.variable("x"), F.f.equivalence(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("(x <=> y) | z")).isEqualTo(F.f.or(F.f.equivalence(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x | (y <=> z)")).isEqualTo(F.f.or(F.f.variable("x"), F.f.equivalence(F.f.variable("y"), F.f.variable("z"))));
        assertThat(parser.parse("x => y <=> z")).isEqualTo(F.f.equivalence(F.f.implication(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")));
        assertThat(parser.parse("x => (y <=> z)")).isEqualTo(F.f.implication(F.f.variable("x"), F.f.equivalence(F.f.variable("y"), F.f.variable("z"))));
    }

    @Test
    public void parseEmptyString() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
        assertThat(parser.parse("")).isEqualTo(F.f.verum());
    }

    @Test
    public void testSkipSymbols() throws ParserException {
        final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
        assertThat(parser.parse(" ")).isEqualTo(F.f.verum());
        assertThat(parser.parse("\t")).isEqualTo(F.f.verum());
        assertThat(parser.parse("\n")).isEqualTo(F.f.verum());
        assertThat(parser.parse("\r")).isEqualTo(F.f.verum());
        assertThat(parser.parse(" \r\n\n  \t")).isEqualTo(F.f.verum());
        assertThat(parser.parse("a\n&\tb")).isEqualTo(F.AND1);
        assertThat(parser.parse(" a\r=>\t\tb")).isEqualTo(F.IMP1);
        assertThat(parser.parse(" 2\n*a\r+\n\n-4*\tb    +3*x=2")).isEqualTo(F.PBC1);
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
        assertThat(F.f.parse("a & b")).isEqualTo(F.f.and(F.f.variable("a"), F.f.variable("b")));
        assertThat(F.f.parse("2*a + -4*b + 3*x = 2")).isEqualTo(F.PBC1);
    }

    @Test
    public void testIllegalVariable1() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("$$%")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalVariable3() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse(";;23")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalVariable4() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("{0}")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator1() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("A + B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator2() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("A &")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator3() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("A /")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator4() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("-A")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalOperator5() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("A * B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalBrackets1() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("(A & B")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula1() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("((A & B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula2() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("(A & (C & D) B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula3() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("A | A + (C | B + C)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula4() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("A | A & (C | B & C")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula5() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("A & ~B)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula6() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("12)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalFormula7() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("ab@cd)")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testIllegalSkipPosition() {
        assertThatThrownBy(() -> new PseudoBooleanParser(F.f).parse("- 1*x <= 3")).isInstanceOf(ParserException.class);
    }

    @Test
    public void testToStrings() {
        assertThat(new PseudoBooleanLexer(null).toString()).isEqualTo("PseudoBooleanLexer");
        assertThat(new PseudoBooleanParser(F.f).toString()).isEqualTo("PseudoBooleanParser");
    }
}
