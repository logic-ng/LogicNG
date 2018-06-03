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

package org.logicng.io.parsers;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.CType;
import org.logicng.formulas.F;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit Tests for the class {@link PseudoBooleanParser}.
 * @version 1.1
 * @since 1.0
 */
public class PseudoBooleanParserTest {

  @Test
  public void testExceptions() throws ParserException {
    final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
    Assert.assertEquals(F.f.verum(), parser.parse(""));
    Assert.assertEquals(F.f.verum(), parser.parse((String) null));
  }

  @Test
  public void testParseConstants() throws ParserException {
    final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
    Assert.assertEquals(F.f.verum(), parser.parse("$true"));
    Assert.assertEquals(F.f.falsum(), parser.parse("$false"));
  }

  @Test
  public void testParseLiterals() throws ParserException {
    final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
    Assert.assertEquals(F.f.variable("A"), parser.parse("A"));
    Assert.assertEquals(F.f.variable("a"), parser.parse("a"));
    Assert.assertEquals(F.f.variable("a1"), parser.parse("a1"));
    Assert.assertEquals(F.f.variable("aA_Bb_Cc_12_3"), parser.parse("aA_Bb_Cc_12_3"));
    Assert.assertEquals(F.f.literal("A", false), parser.parse("~A"));
    Assert.assertEquals(F.f.literal("a", false), parser.parse("~a"));
    Assert.assertEquals(F.f.literal("aA_Bb_Cc_12_3", false), parser.parse("~aA_Bb_Cc_12_3"));
  }

  @Test
  public void testParseOperators() throws ParserException {
    final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
    Assert.assertEquals(F.f.not(F.f.variable("a")), parser.parse("~a"));
    Assert.assertEquals(F.f.not(F.f.variable("Var")), parser.parse("~Var"));
    Assert.assertEquals(F.f.and(F.f.variable("a"), F.f.variable("b")), parser.parse("a & b"));
    Assert.assertEquals(F.f.and(F.f.literal("a", false), F.f.literal("b", false)), parser.parse("~a & ~b"));
    Assert.assertEquals(F.f.and(F.f.literal("a", false), F.f.variable("b"), F.f.literal("c", false), F.f.variable("d")), parser.parse("~a & b & ~c & d"));
    Assert.assertEquals(F.f.or(F.f.variable("a"), F.f.variable("b")), parser.parse("a | b"));
    Assert.assertEquals(F.f.or(F.f.literal("a", false), F.f.literal("b", false)), parser.parse("~a | ~b"));
    Assert.assertEquals(F.f.or(F.f.literal("a", false), F.f.variable("b"), F.f.literal("c", false), F.f.variable("d")), parser.parse("~a | b | ~c | d"));
    Assert.assertEquals(F.f.implication(F.f.variable("a"), F.f.variable("b")), parser.parse("a => b"));
    Assert.assertEquals(F.f.implication(F.f.literal("a", false), F.f.literal("b", false)), parser.parse("~a => ~b"));
    Assert.assertEquals(F.f.equivalence(F.f.variable("a"), F.f.variable("b")), parser.parse("a <=> b"));
    Assert.assertEquals(F.f.equivalence(F.f.literal("a", false), F.f.literal("b", false)), parser.parse("~a <=> ~b"));
  }

  @Test
  public void testParseMultiplication() throws ParserException {
    final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
    Assert.assertEquals(F.f.pbc(CType.EQ, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}), parser.parse("13 * abc = 4"));
    Assert.assertEquals(F.f.pbc(CType.EQ, 4, new Literal[]{F.f.variable("a")}, new int[]{-13}), parser.parse("-13 * a = 4"));
    Assert.assertEquals(F.f.pbc(CType.EQ, -442, new Literal[]{F.f.literal("abc", false)}, new int[]{13}), parser.parse("13 * ~abc = -442"));
    Assert.assertEquals(F.f.pbc(CType.EQ, -442, new Literal[]{F.f.literal("a", false)}, new int[]{-13}), parser.parse("-13 * ~a = -442"));
    Assert.assertEquals(F.f.pbc(CType.EQ, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}), parser.parse("13 * abc = 4"));
    Assert.assertEquals(F.f.pbc(CType.GT, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}), parser.parse("13 * abc > 4"));
    Assert.assertEquals(F.f.pbc(CType.GE, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}), parser.parse("13 * abc >= 4"));
    Assert.assertEquals(F.f.pbc(CType.LT, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}), parser.parse("13 * abc < 4"));
    Assert.assertEquals(F.f.pbc(CType.LE, 4, new Literal[]{F.f.variable("abc")}, new int[]{13}), parser.parse("13 * abc <= 4"));
  }

  @Test
  public void testParseAddition() throws ParserException {
    final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
    Assert.assertEquals(F.f.pbc(CType.LT, -4, new Literal[]{F.f.variable("c"), F.f.literal("d", false)}, new int[]{4, -4}), parser.parse("4 * c + -4 * ~d < -4"));
    Assert.assertEquals(F.f.pbc(CType.GE, -5, new Literal[]{F.f.variable("c"), F.f.literal("c", false)}, new int[]{5, -5}), parser.parse("5 * c + -5 * ~c >= -5"));
    Assert.assertEquals(F.f.pbc(CType.GT, -6, new Literal[]{F.f.variable("a"), F.f.literal("b", false), F.f.literal("c", false)}, new int[]{6, -6, 12}), parser.parse("6 * a + -6 * ~b + 12 * ~c > -6"));
    Assert.assertEquals(F.f.pbc(CType.LT, -4, new Literal[]{F.f.variable("c"), F.f.literal("d", false)}, new int[]{1, -4}), parser.parse("c + -4 * ~d < -4"));
    Assert.assertEquals(F.f.pbc(CType.GE, -5, new Literal[]{F.f.variable("c"), F.f.literal("c", false)}, new int[]{5, 1}), parser.parse("5 * c + ~c >= -5"));
    Assert.assertEquals(F.f.pbc(CType.GE, -5, new Literal[]{F.f.variable("c"), F.f.literal("d", true)}, new int[]{1, 1}), parser.parse("c + d >= -5"));
    Assert.assertEquals(F.f.pbc(CType.GE, -5, new Literal[]{F.f.literal("c", false), F.f.literal("d", false)}, new int[]{1, 1}), parser.parse("~c + ~d >= -5"));
    Assert.assertEquals(F.f.pbc(CType.EQ, -5, new Literal[]{F.f.literal("c", false)}, new int[]{1}), parser.parse("~c = -5"));
    Assert.assertEquals(F.f.not(F.f.pbc(CType.EQ, -5, new Literal[]{F.f.literal("c", true)}, new int[]{1})), parser.parse("~(c = -5)"));
  }

  @Test
  public void testCombination() throws ParserException {
    final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
    final Formula pbc = F.f.pbc(CType.GT, -6, new Literal[]{F.f.variable("a"), F.f.literal("b", false), F.f.literal("c", false)}, new int[]{6, -6, 12});
    Assert.assertEquals(F.f.and(F.f.implication(F.f.variable("x"), F.f.and(F.f.variable("y"), F.f.variable("z"))), pbc), parser.parse("(x => y & z) & (6 * a + -6 * ~b + 12 * ~c > -6)"));
    Assert.assertEquals(F.f.not(pbc), parser.parse("~(6 * a - 6 * ~b - -12 * ~c > -6)"));
  }

  @Test
  public void testParsePrecedences() throws ParserException {
    final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
    Assert.assertEquals(F.f.or(F.f.variable("x"), F.f.and(F.f.variable("y"), F.f.variable("z"))), parser.parse("x | y & z"));
    Assert.assertEquals(F.f.or(F.f.and(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("x & y | z"));
    Assert.assertEquals(F.f.implication(F.f.variable("x"), F.f.and(F.f.variable("y"), F.f.variable("z"))), parser.parse("x => y & z"));
    Assert.assertEquals(F.f.implication(F.f.and(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("x & y => z"));
    Assert.assertEquals(F.f.equivalence(F.f.variable("x"), F.f.and(F.f.variable("y"), F.f.variable("z"))), parser.parse("x <=> y & z"));
    Assert.assertEquals(F.f.equivalence(F.f.and(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("x & y <=> z"));
    Assert.assertEquals(F.f.implication(F.f.variable("x"), F.f.or(F.f.variable("y"), F.f.variable("z"))), parser.parse("x => y | z"));
    Assert.assertEquals(F.f.implication(F.f.or(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("x | y => z"));
    Assert.assertEquals(F.f.equivalence(F.f.variable("x"), F.f.or(F.f.variable("y"), F.f.variable("z"))), parser.parse("x <=> y | z"));
    Assert.assertEquals(F.f.equivalence(F.f.or(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("x | y <=> z"));
    Assert.assertEquals(F.f.implication(F.f.variable("x"), F.f.implication(F.f.variable("y"), F.f.variable("z"))), parser.parse("x => y => z"));
    Assert.assertEquals(F.f.equivalence(F.f.variable("x"), F.f.equivalence(F.f.variable("y"), F.f.variable("z"))), parser.parse("x <=> y <=> z"));
    Assert.assertEquals(F.f.and(F.f.or(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("(x | y) & z"));
    Assert.assertEquals(F.f.and(F.f.variable("x"), F.f.or(F.f.variable("y"), F.f.variable("z"))), parser.parse("x & (y | z)"));
    Assert.assertEquals(F.f.and(F.f.implication(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("(x => y) & z"));
    Assert.assertEquals(F.f.and(F.f.variable("x"), F.f.implication(F.f.variable("y"), F.f.variable("z"))), parser.parse("x & (y => z)"));
    Assert.assertEquals(F.f.or(F.f.implication(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("(x => y) | z"));
    Assert.assertEquals(F.f.or(F.f.variable("x"), F.f.implication(F.f.variable("y"), F.f.variable("z"))), parser.parse("x | (y => z)"));
    Assert.assertEquals(F.f.and(F.f.equivalence(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("(x <=> y) & z"));
    Assert.assertEquals(F.f.and(F.f.variable("x"), F.f.equivalence(F.f.variable("y"), F.f.variable("z"))), parser.parse("x & (y <=> z)"));
    Assert.assertEquals(F.f.or(F.f.equivalence(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("(x <=> y) | z"));
    Assert.assertEquals(F.f.or(F.f.variable("x"), F.f.equivalence(F.f.variable("y"), F.f.variable("z"))), parser.parse("x | (y <=> z)"));
    Assert.assertEquals(F.f.equivalence(F.f.implication(F.f.variable("x"), F.f.variable("y")), F.f.variable("z")), parser.parse("x => y <=> z"));
    Assert.assertEquals(F.f.implication(F.f.variable("x"), F.f.equivalence(F.f.variable("y"), F.f.variable("z"))), parser.parse("x => (y <=> z)"));
  }

  @Test
  public void parseEmptyString() throws ParserException {
    final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
    Assert.assertEquals(F.f.verum(), parser.parse(""));
  }

  @Test
  public void testSkipSymbols() throws ParserException {
    final PseudoBooleanParser parser = new PseudoBooleanParser(F.f);
    Assert.assertEquals(F.f.verum(), parser.parse(" "));
    Assert.assertEquals(F.f.verum(), parser.parse("\t"));
    Assert.assertEquals(F.f.verum(), parser.parse("\n"));
    Assert.assertEquals(F.f.verum(), parser.parse("\r"));
    Assert.assertEquals(F.f.verum(), parser.parse(" \r\n\n  \t"));
    Assert.assertEquals(F.AND1, parser.parse("a\n&\tb"));
    Assert.assertEquals(F.IMP1, parser.parse(" a\r=>\t\tb"));
    Assert.assertEquals(F.PBC1, parser.parse(" 2\n*a\r+\n\n-4*\tb    +3*x=2"));
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
    Assert.assertEquals(F.f.and(F.f.variable("a"), F.f.variable("b")), F.f.parse("a & b"));
    Assert.assertEquals(F.PBC1, F.f.parse("2*a + -4*b + 3*x = 2"));
  }

  @Test(expected = ParserException.class)
  public void testIllegalVariable1() throws ParserException {
    new PseudoBooleanParser(F.f).parse("$$%");
  }

  @Test(expected = ParserException.class)
  public void testIllegalVariable3() throws ParserException {
    new PseudoBooleanParser(F.f).parse(";;23");
  }

  @Test(expected = ParserException.class)
  public void testIllegalVariable4() throws ParserException {
    new PseudoBooleanParser(F.f).parse("{0}");
  }

  @Test(expected = ParserException.class)
  public void testIllegalOperator1() throws ParserException {
    new PseudoBooleanParser(F.f).parse("A + B");
  }

  @Test(expected = ParserException.class)
  public void testIllegalOperator2() throws ParserException {
    new PseudoBooleanParser(F.f).parse("A &");
  }

  @Test(expected = ParserException.class)
  public void testIllegalOperator3() throws ParserException {
    new PseudoBooleanParser(F.f).parse("A /");
  }

  @Test(expected = ParserException.class)
  public void testIllegalOperator4() throws ParserException {
    new PseudoBooleanParser(F.f).parse("-A");
  }

  @Test(expected = ParserException.class)
  public void testIllegalOperator5() throws ParserException {
    new PseudoBooleanParser(F.f).parse("A * B");
  }

  @Test(expected = ParserException.class)
  public void testIllegalBrackets1() throws ParserException {
    new PseudoBooleanParser(F.f).parse("(A & B");
  }

  @Test(expected = ParserException.class)
  public void testIllegalFormula1() throws ParserException {
    new PseudoBooleanParser(F.f).parse("((A & B)");
  }

  @Test(expected = ParserException.class)
  public void testIllegalFormula2() throws ParserException {
    new PseudoBooleanParser(F.f).parse("(A & (C & D) B)");
  }

  @Test(expected = ParserException.class)
  public void testIllegalFormula3() throws ParserException {
    new PseudoBooleanParser(F.f).parse("A | A + (C | B + C)");
  }

  @Test(expected = ParserException.class)
  public void testIllegalFormula4() throws ParserException {
    new PseudoBooleanParser(F.f).parse("A | A & (C | B & C");
  }

  @Test(expected = ParserException.class)
  public void testIllegalFormula5() throws ParserException {
    new PseudoBooleanParser(F.f).parse("A & ~B)");
  }

  @Test(expected = ParserException.class)
  public void testIllegalFormula7() throws ParserException {
    new PseudoBooleanParser(F.f).parse("abc@");
  }

  @Test(expected = ParserException.class)
  public void testIllegalSkipPosition() throws ParserException {
    new PseudoBooleanParser(F.f).parse("- 1*x <= 3");
  }

  @Test
  public void testToStrings() {
    Assert.assertEquals("PseudoBooleanLexer", new PseudoBooleanLexer(null).toString());
    Assert.assertEquals("PseudoBooleanParser", new PseudoBooleanParser(F.f).toString());
  }
}
