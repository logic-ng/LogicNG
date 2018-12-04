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

package org.logicng.transformations.cnf;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;

/**
 * Unit tests for the class {@link CNFEncoder}.
 * @version 1.1
 * @since 1.1
 */
public class CNFEncoderTest {

  private static final String p1 = "(x1 | x2) & x3 & x4 & ((x1 & x5 & ~(x6 | x7) | x8) | x9)";
  private static final String p2 = "(y1 | y2) & y3 & y4 & ((y1 & y5 & ~(y6 | y7) | y8) | y9)";
  private static final String p3 = "(z1 | z2) & z3 & z4 & ((z1 & z5 & ~(z6 | z7) | z8) | z9)";

  @Test
  public void testFactorization() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula phi1 = p.parse(p1);
    Assert.assertEquals(10, phi1.numberOfAtoms());
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), phi1.cnf());
    f.putConfiguration(new CNFConfig.Builder().build());
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), phi1.cnf());
    f.putConfiguration(new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.FACTORIZATION).build());
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), phi1.cnf());
    CNFEncoder encoder = new CNFEncoder(f, new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.FACTORIZATION).build());
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), encoder.encode(phi1));
  }

  @Test
  public void testTseitin() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula phi1 = p.parse(p1);
    final Formula phi2 = p.parse(p2);
    f.putConfiguration(new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.TSEITIN).build());
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), phi1.cnf());
    f.putConfiguration(new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.TSEITIN).atomBoundary(8).build());
    Assert.assertEquals(p.parse("(@RESERVED_CNF_0 | ~x1) & (@RESERVED_CNF_0 | ~x2) & (~@RESERVED_CNF_0 | x1 | x2) & (~@RESERVED_CNF_1 | x1) & (~@RESERVED_CNF_1 | x5) & (~@RESERVED_CNF_1 | ~x6) & (~@RESERVED_CNF_1 | ~x7) & (@RESERVED_CNF_1 | ~x1 | ~x5 | x6 | x7) & (@RESERVED_CNF_2 | ~@RESERVED_CNF_1) & (@RESERVED_CNF_2 | ~x8) & (@RESERVED_CNF_2 | ~x9) & (~@RESERVED_CNF_2 | @RESERVED_CNF_1 | x8 | x9) & @RESERVED_CNF_0 & x3 & x4 & @RESERVED_CNF_2"), phi1.cnf());
    f.putConfiguration(new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.TSEITIN).atomBoundary(11).build());
    Assert.assertEquals(p.parse("(y1 | y2) & y3 & y4 & (y1 | y8 | y9) & (y5 | y8 | y9) & (~y6 | y8 | y9) & (~y7 | y8 | y9)"), phi2.cnf());
  }

  @Test
  public void testPG() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula phi1 = p.parse(p1);
    final Formula phi2 = p.parse(p2);
    f.putConfiguration(new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), phi1.cnf());
    f.putConfiguration(new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).atomBoundary(8).build());
    Assert.assertEquals(p.parse("@RESERVED_CNF_1 & x3 & x4 & @RESERVED_CNF_2 & (~@RESERVED_CNF_1 | x1 | x2) & (~@RESERVED_CNF_2 | @RESERVED_CNF_3 | x8 | x9) & (~@RESERVED_CNF_3 | x1) & (~@RESERVED_CNF_3 | x5) & (~@RESERVED_CNF_3 | ~x6) & (~@RESERVED_CNF_3 | ~x7)"), phi1.cnf());
    f.putConfiguration(new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).atomBoundary(11).build());
    Assert.assertEquals(p.parse("(y1 | y2) & y3 & y4 & (y1 | y8 | y9) & (y5 | y8 | y9) & (~y6 | y8 | y9) & (~y7 | y8 | y9)"), phi2.cnf());
  }

  @Test
  public void testAdvanced() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula phi1 = p.parse(p1);
    final Formula phi2 = p.parse(p2);
    final Formula phi3 = p.parse(p3);
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), phi1.cnf());
    f.putConfiguration(new CNFConfig.Builder().createdClauseBoundary(5).atomBoundary(3).build());
    Assert.assertEquals(p.parse("(y1 | y2) & y3 & y4 & (~@RESERVED_CNF_0 | y1) & (~@RESERVED_CNF_0 | y5) & (~@RESERVED_CNF_0 | ~y6) & (~@RESERVED_CNF_0 | ~y7) & (@RESERVED_CNF_0 | ~y1 | ~y5 | y6 | y7) & (@RESERVED_CNF_0 | y8 | y9)"), phi2.cnf());
    f.putConfiguration(new CNFConfig.Builder().createdClauseBoundary(-1).distributionBoundary(5).atomBoundary(3).build());
    Assert.assertEquals(p.parse("(z1 | z2) & z3 & z4 & (~@RESERVED_CNF_2 | z1) & (~@RESERVED_CNF_2 | z5) & (~@RESERVED_CNF_2 | ~z6) & (~@RESERVED_CNF_2 | ~z7) & (@RESERVED_CNF_2 | ~z1 | ~z5 | z6 | z7) & (@RESERVED_CNF_2 | z8 | z9)"), phi3.cnf());
  }

  @Test
  public void testAdvancedWithPGFallback() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula phi1 = p.parse(p1);
    final Formula phi2 = p.parse(p2);
    final Formula phi3 = p.parse(p3);
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), phi1.cnf());
    f.putConfiguration(new CNFConfig.Builder().createdClauseBoundary(5).atomBoundary(3).fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
    Assert.assertEquals(p.parse("(y1 | y2) & y3 & y4 & (@RESERVED_CNF_1 | y8 | y9) & (~@RESERVED_CNF_1 | y1) & (~@RESERVED_CNF_1 | y5) & (~@RESERVED_CNF_1 | ~y6) & (~@RESERVED_CNF_1 | ~y7)"), phi2.cnf());
    f.putConfiguration(new CNFConfig.Builder().createdClauseBoundary(-1).distributionBoundary(5).atomBoundary(3).fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
    Assert.assertEquals(p.parse("(z1 | z2) & z3 & z4 & (@RESERVED_CNF_3 | z8 | z9) & (~@RESERVED_CNF_3 | z1) & (~@RESERVED_CNF_3 | z5) & (~@RESERVED_CNF_3 | ~z6) & (~@RESERVED_CNF_3 | ~z7)"), phi3.cnf());
  }

  @Test
  public void testTseitinEncoder() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula phi1 = p.parse(p1);
    CNFEncoder encoder1 = new CNFEncoder(f, new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.TSEITIN).build());
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), encoder1.encode(phi1));
    CNFEncoder encoder2 = new CNFEncoder(f, new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.TSEITIN).atomBoundary(8).build());
    Assert.assertEquals(p.parse("(@RESERVED_CNF_0 | ~x1) & (@RESERVED_CNF_0 | ~x2) & (~@RESERVED_CNF_0 | x1 | x2) & (~@RESERVED_CNF_1 | x1) & (~@RESERVED_CNF_1 | x5) & (~@RESERVED_CNF_1 | ~x6) & (~@RESERVED_CNF_1 | ~x7) & (@RESERVED_CNF_1 | ~x1 | ~x5 | x6 | x7) & (@RESERVED_CNF_2 | ~@RESERVED_CNF_1) & (@RESERVED_CNF_2 | ~x8) & (@RESERVED_CNF_2 | ~x9) & (~@RESERVED_CNF_2 | @RESERVED_CNF_1 | x8 | x9) & @RESERVED_CNF_0 & x3 & x4 & @RESERVED_CNF_2"), encoder2.encode(phi1));
  }

  @Test
  public void testPGEncoder() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula phi1 = p.parse(p1);
    CNFEncoder encoder1 = new CNFEncoder(f, new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), encoder1.encode(phi1));
    CNFEncoder encoder2 = new CNFEncoder(f, new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).atomBoundary(8).build());
    Assert.assertEquals(p.parse("@RESERVED_CNF_1 & x3 & x4 & @RESERVED_CNF_2 & (~@RESERVED_CNF_1 | x1 | x2) & (~@RESERVED_CNF_2 | @RESERVED_CNF_3 | x8 | x9) & (~@RESERVED_CNF_3 | x1) & (~@RESERVED_CNF_3 | x5) & (~@RESERVED_CNF_3 | ~x6) & (~@RESERVED_CNF_3 | ~x7)"), encoder2.encode(phi1));
  }

  @Test
  public void testAdvancedEncoder() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser p = new PropositionalParser(f);
    final Formula phi1 = p.parse(p1);
    final Formula phi2 = p.parse(p2);
    final Formula phi3 = p.parse(p3);
    CNFEncoder encoder1 = new CNFEncoder(f, new CNFConfig.Builder().build());
    Assert.assertEquals(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"), encoder1.encode(phi1));
    CNFEncoder encoder2 = new CNFEncoder(f, new CNFConfig.Builder().createdClauseBoundary(5).atomBoundary(3).build());
    Assert.assertEquals(p.parse("(y1 | y2) & y3 & y4 & (~@RESERVED_CNF_0 | y1) & (~@RESERVED_CNF_0 | y5) & (~@RESERVED_CNF_0 | ~y6) & (~@RESERVED_CNF_0 | ~y7) & (@RESERVED_CNF_0 | ~y1 | ~y5 | y6 | y7) & (@RESERVED_CNF_0 | y8 | y9)"), encoder2.encode(phi2));
    CNFEncoder encoder3 = new CNFEncoder(f, new CNFConfig.Builder().createdClauseBoundary(-1).distributionBoundary(5).atomBoundary(3).build());
    Assert.assertEquals(p.parse("(z1 | z2) & z3 & z4 & (~@RESERVED_CNF_2 | z1) & (~@RESERVED_CNF_2 | z5) & (~@RESERVED_CNF_2 | ~z6) & (~@RESERVED_CNF_2 | ~z7) & (@RESERVED_CNF_2 | ~z1 | ~z5 | z6 | z7) & (@RESERVED_CNF_2 | z8 | z9)"), encoder3.encode(phi3));
  }

  @Test
  public void testStrings() {
    String expected = String.format("CNFConfig{%n" +
            "algorithm=TSEITIN%n" +
            "fallbackAlgorithmForAdvancedEncoding=PLAISTED_GREENBAUM%n" +
            "distributedBoundary=-1%n" +
            "createdClauseBoundary=1000%n" +
            "atomBoundary=12%n" +
            "}%n");
    FormulaFactory f = new FormulaFactory();
    CNFConfig config = new CNFConfig.Builder().algorithm(CNFConfig.Algorithm.TSEITIN).fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build();
    CNFEncoder encoder = new CNFEncoder(f, config);
    Assert.assertEquals(expected, config.toString());
    Assert.assertEquals(expected, encoder.toString());
    Assert.assertEquals(CNFConfig.Algorithm.TSEITIN, CNFConfig.Algorithm.valueOf("TSEITIN"));
  }

  @Test
  public void testBugIssueNo4() throws ParserException {
    final FormulaFactory f = new FormulaFactory();
    final PropositionalParser parser = new PropositionalParser(f);
    final Formula f1 = parser.parse("(x10 & x9 & x3 & x12 | x10 & x9 & x8 | x9 & x8 & x12) & ~x5 & ~x7 & x1 | (x10 & x9 & x3 & x12 | x10 & x9 & x8 | x9 & x8 & x12) & ~(x11 & x3) & ~(x11 & x8) & ~x5 & ~x7 & x0");
    final Formula f2 = parser.parse("x1 & x3 & x4");
    final Formula f3 = parser.parse("(x10 & x9 & x3 & x12 | x10 & x9 & x8 | x9 & x8 & x12) & ~(x11 & x3) & ~(x11 & x8 & x12) & ~x5 & ~x7 & x1 | (x10 & x9 & x3 & x12 | x10 & x9 & x8 | x9 & x8 & x12) & ~(x11 & x3) & ~(x11 & x8) & ~x5 & ~x7 & x0 | x3 & x4 & ~x5 & ~x7 & x1 | x3 & x4 & ~x5 & ~x7 & x0 | x2 & x6 & ~x5 & ~x7 & x0");
    final Formula f4 = parser.parse("(x1 & x3 & x4 | x0 & (x2 & x6 | x3 & x4) | x9 & (x1 & x10 & x8 & ~x12 & x3 | (x1 | x0) & (x12 & (x10 & x3 | x8) | x10 & x8) & ~x11)) & ~x5 & ~x7");
    Assert.assertNotEquals(null, f.not(f.equivalence(f1, f2)).cnf());
    Assert.assertNotEquals(null, f.not(f.equivalence(f3, f4)).cnf());
  }

  @Test(expected = IllegalArgumentException.class)
  public void testWrongFallbackForConfig() {
    new CNFConfig.Builder().fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.FACTORIZATION).build();
  }

}
