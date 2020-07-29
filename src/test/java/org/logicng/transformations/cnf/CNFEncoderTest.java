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

package org.logicng.transformations.cnf;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.predicates.CNFPredicate;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.List;
import java.util.SortedSet;

/**
 * Unit tests for the class {@link CNFEncoder}.
 * @version 2.0.0
 * @since 1.1
 */
public class CNFEncoderTest {

    private static final CNFPredicate cnfPredicate = CNFPredicate.get();

    private static final String p1 = "(x1 | x2) & x3 & x4 & ((x1 & x5 & ~(x6 | x7) | x8) | x9)";
    private static final String p2 = "(y1 | y2) & y3 & y4 & ((y1 & y5 & ~(y6 | y7) | y8) | y9)";
    private static final String p3 = "(z1 | z2) & z3 & z4 & ((z1 & z5 & ~(z6 | z7) | z8) | z9)";

    @Test
    public void testFactorization() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Formula phi1 = p.parse(p1);
        assertThat(phi1.numberOfAtoms()).isEqualTo(10);
        assertThat(phi1.cnf()).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
        f.putConfiguration(CNFConfig.builder().build());
        assertThat(phi1.cnf()).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
        f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.FACTORIZATION).build());
        assertThat(phi1.cnf()).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
        final CNFEncoder encoder = new CNFEncoder(f, CNFConfig.builder().algorithm(CNFConfig.Algorithm.FACTORIZATION).build());
        assertThat(encoder.encode(phi1)).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
    }

    @Test
    public void testTseitin() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Formula phi1 = p.parse(p1);
        final Formula phi2 = p.parse(p2);
        f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.TSEITIN).build());
        assertThat(phi1.cnf()).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
        f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.TSEITIN).atomBoundary(8).build());
        assertThat(phi1.cnf()).isEqualTo(p.parse("(@RESERVED_CNF_0 | ~x1) & (@RESERVED_CNF_0 | ~x2) & (~@RESERVED_CNF_0 | x1 | x2) & (~@RESERVED_CNF_1 | x1) & (~@RESERVED_CNF_1 | x5) & (~@RESERVED_CNF_1 | ~x6) & (~@RESERVED_CNF_1 | ~x7) & (@RESERVED_CNF_1 | ~x1 | ~x5 | x6 | x7) & (@RESERVED_CNF_2 | ~@RESERVED_CNF_1) & (@RESERVED_CNF_2 | ~x8) & (@RESERVED_CNF_2 | ~x9) & (~@RESERVED_CNF_2 | @RESERVED_CNF_1 | x8 | x9) & @RESERVED_CNF_0 & x3 & x4 & @RESERVED_CNF_2"));
        f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.TSEITIN).atomBoundary(11).build());
        assertThat(phi2.cnf()).isEqualTo(p.parse("(y1 | y2) & y3 & y4 & (y1 | y8 | y9) & (y5 | y8 | y9) & (~y6 | y8 | y9) & (~y7 | y8 | y9)"));
    }

    @Test
    public void testPG() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Formula phi1 = p.parse(p1);
        final Formula phi2 = p.parse(p2);
        f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
        assertThat(phi1.cnf()).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
        f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).atomBoundary(8).build());
        assertThat(phi1.cnf()).isEqualTo(p.parse("@RESERVED_CNF_1 & x3 & x4 & @RESERVED_CNF_2 & (~@RESERVED_CNF_1 | x1 | x2) & (~@RESERVED_CNF_2 | @RESERVED_CNF_3 | x8 | x9) & (~@RESERVED_CNF_3 | x1) & (~@RESERVED_CNF_3 | x5) & (~@RESERVED_CNF_3 | ~x6) & (~@RESERVED_CNF_3 | ~x7)"));
        f.putConfiguration(CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).atomBoundary(11).build());
        assertThat(phi2.cnf()).isEqualTo(p.parse("(y1 | y2) & y3 & y4 & (y1 | y8 | y9) & (y5 | y8 | y9) & (~y6 | y8 | y9) & (~y7 | y8 | y9)"));
    }

    @Test
    public void testAdvanced() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Formula phi1 = p.parse(p1);
        final Formula phi2 = p.parse(p2);
        final Formula phi3 = p.parse(p3);
        assertThat(phi1.cnf()).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
        f.putConfiguration(CNFConfig.builder().createdClauseBoundary(5).atomBoundary(3).build());
        assertThat(phi2.cnf()).isEqualTo(p.parse("(y1 | y2) & y3 & y4 & (~@RESERVED_CNF_0 | y1) & (~@RESERVED_CNF_0 | y5) & (~@RESERVED_CNF_0 | ~y6) & (~@RESERVED_CNF_0 | ~y7) & (@RESERVED_CNF_0 | ~y1 | ~y5 | y6 | y7) & (@RESERVED_CNF_0 | y8 | y9)"));
        f.putConfiguration(CNFConfig.builder().createdClauseBoundary(-1).distributionBoundary(5).atomBoundary(3).build());
        assertThat(phi3.cnf()).isEqualTo(p.parse("(z1 | z2) & z3 & z4 & (~@RESERVED_CNF_2 | z1) & (~@RESERVED_CNF_2 | z5) & (~@RESERVED_CNF_2 | ~z6) & (~@RESERVED_CNF_2 | ~z7) & (@RESERVED_CNF_2 | ~z1 | ~z5 | z6 | z7) & (@RESERVED_CNF_2 | z8 | z9)"));
    }

    @Test
    public void testAdvancedWithPGFallback() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Formula phi1 = p.parse(p1);
        final Formula phi2 = p.parse(p2);
        final Formula phi3 = p.parse(p3);
        assertThat(phi1.cnf()).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
        f.putConfiguration(CNFConfig.builder().createdClauseBoundary(5).atomBoundary(3).fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
        assertThat(phi2.cnf()).isEqualTo(p.parse("(y1 | y2) & y3 & y4 & (@RESERVED_CNF_1 | y8 | y9) & (~@RESERVED_CNF_1 | y1) & (~@RESERVED_CNF_1 | y5) & (~@RESERVED_CNF_1 | ~y6) & (~@RESERVED_CNF_1 | ~y7)"));
        f.putConfiguration(CNFConfig.builder().createdClauseBoundary(-1).distributionBoundary(5).atomBoundary(3).fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
        assertThat(phi3.cnf()).isEqualTo(p.parse("(z1 | z2) & z3 & z4 & (@RESERVED_CNF_3 | z8 | z9) & (~@RESERVED_CNF_3 | z1) & (~@RESERVED_CNF_3 | z5) & (~@RESERVED_CNF_3 | ~z6) & (~@RESERVED_CNF_3 | ~z7)"));
    }

    @Test
    public void testTseitinEncoder() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Formula phi1 = p.parse(p1);
        final CNFEncoder encoder1 = new CNFEncoder(f, CNFConfig.builder().algorithm(CNFConfig.Algorithm.TSEITIN).build());
        assertThat(encoder1.encode(phi1)).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
        final CNFEncoder encoder2 = new CNFEncoder(f, CNFConfig.builder().algorithm(CNFConfig.Algorithm.TSEITIN).atomBoundary(8).build());
        assertThat(encoder2.encode(phi1)).isEqualTo(p.parse("(@RESERVED_CNF_0 | ~x1) & (@RESERVED_CNF_0 | ~x2) & (~@RESERVED_CNF_0 | x1 | x2) & (~@RESERVED_CNF_1 | x1) & (~@RESERVED_CNF_1 | x5) & (~@RESERVED_CNF_1 | ~x6) & (~@RESERVED_CNF_1 | ~x7) & (@RESERVED_CNF_1 | ~x1 | ~x5 | x6 | x7) & (@RESERVED_CNF_2 | ~@RESERVED_CNF_1) & (@RESERVED_CNF_2 | ~x8) & (@RESERVED_CNF_2 | ~x9) & (~@RESERVED_CNF_2 | @RESERVED_CNF_1 | x8 | x9) & @RESERVED_CNF_0 & x3 & x4 & @RESERVED_CNF_2"));
    }

    @Test
    public void testPGEncoder() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Formula phi1 = p.parse(p1);
        final CNFEncoder encoder1 = new CNFEncoder(f, CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build());
        assertThat(encoder1.encode(phi1)).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
        final CNFEncoder encoder2 = new CNFEncoder(f, CNFConfig.builder().algorithm(CNFConfig.Algorithm.PLAISTED_GREENBAUM).atomBoundary(8).build());
        assertThat(encoder2.encode(phi1)).isEqualTo(p.parse("@RESERVED_CNF_1 & x3 & x4 & @RESERVED_CNF_2 & (~@RESERVED_CNF_1 | x1 | x2) & (~@RESERVED_CNF_2 | @RESERVED_CNF_3 | x8 | x9) & (~@RESERVED_CNF_3 | x1) & (~@RESERVED_CNF_3 | x5) & (~@RESERVED_CNF_3 | ~x6) & (~@RESERVED_CNF_3 | ~x7)"));
    }

    @Test
    public void testBDDEncoder() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Formula phi1 = p.parse(p1);
        final Formula phi2 = p.parse(p2);
        final Formula phi3 = p.parse(p3);
        final CNFEncoder encoder = new CNFEncoder(f, CNFConfig.builder().algorithm(CNFConfig.Algorithm.BDD).build());
        final Formula phi1CNF = encoder.encode(phi1);
        assertThat(phi1CNF.holds(cnfPredicate)).isTrue();
        assertThat(equivalentModels(phi1, phi1CNF, phi1.variables())).isTrue();
        final Formula phi2CNF = encoder.encode(phi2);
        assertThat(phi2CNF.holds(cnfPredicate)).isTrue();
        assertThat(equivalentModels(phi2, phi2CNF, phi2.variables())).isTrue();
        final Formula phi3CNF = encoder.encode(phi3);
        assertThat(phi3CNF.holds(cnfPredicate)).isTrue();
        assertThat(equivalentModels(phi3, phi3CNF, phi3.variables())).isTrue();
    }

    @Test
    public void testAdvancedEncoder() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Formula phi1 = p.parse(p1);
        final Formula phi2 = p.parse(p2);
        final Formula phi3 = p.parse(p3);
        final CNFEncoder encoder1 = new CNFEncoder(f, CNFConfig.builder().build());
        assertThat(encoder1.encode(phi1)).isEqualTo(p.parse("(x1 | x2) & x3 & x4 & (x1 | x8 | x9) & (x5 | x8 | x9) & (~x6 | x8 | x9) & (~x7 | x8 | x9)"));
        final CNFEncoder encoder2 = new CNFEncoder(f, CNFConfig.builder().createdClauseBoundary(5).atomBoundary(3).build());
        assertThat(encoder2.encode(phi2)).isEqualTo(p.parse("(y1 | y2) & y3 & y4 & (~@RESERVED_CNF_0 | y1) & (~@RESERVED_CNF_0 | y5) & (~@RESERVED_CNF_0 | ~y6) & (~@RESERVED_CNF_0 | ~y7) & (@RESERVED_CNF_0 | ~y1 | ~y5 | y6 | y7) & (@RESERVED_CNF_0 | y8 | y9)"));
        final CNFEncoder encoder3 = new CNFEncoder(f, CNFConfig.builder().createdClauseBoundary(-1).distributionBoundary(5).atomBoundary(3).build());
        assertThat(encoder3.encode(phi3)).isEqualTo(p.parse("(z1 | z2) & z3 & z4 & (~@RESERVED_CNF_2 | z1) & (~@RESERVED_CNF_2 | z5) & (~@RESERVED_CNF_2 | ~z6) & (~@RESERVED_CNF_2 | ~z7) & (@RESERVED_CNF_2 | ~z1 | ~z5 | z6 | z7) & (@RESERVED_CNF_2 | z8 | z9)"));
    }

    @Test
    public void testStrings() {
        final String expected = String.format("CNFConfig{%n" +
                "algorithm=TSEITIN%n" +
                "fallbackAlgorithmForAdvancedEncoding=PLAISTED_GREENBAUM%n" +
                "distributedBoundary=-1%n" +
                "createdClauseBoundary=1000%n" +
                "atomBoundary=12%n" +
                "}%n");
        final FormulaFactory f = new FormulaFactory();
        final CNFConfig config = CNFConfig.builder().algorithm(CNFConfig.Algorithm.TSEITIN).fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.PLAISTED_GREENBAUM).build();
        final CNFEncoder encoder = new CNFEncoder(f, config);
        assertThat(config.toString()).isEqualTo(expected);
        assertThat(encoder.toString()).isEqualTo(expected);
        assertThat(CNFConfig.Algorithm.valueOf("TSEITIN")).isEqualTo(CNFConfig.Algorithm.TSEITIN);
    }

    @Test
    public void testBugIssueNo4() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser parser = new PropositionalParser(f);
        final Formula f1 = parser.parse("(x10 & x9 & x3 & x12 | x10 & x9 & x8 | x9 & x8 & x12) & ~x5 & ~x7 & x1 | (x10 & x9 & x3 & x12 | x10 & x9 & x8 | x9 & x8 & x12) & ~(x11 & x3) & ~(x11 & x8) & ~x5 & ~x7 & x0");
        final Formula f2 = parser.parse("x1 & x3 & x4");
        final Formula f3 = parser.parse("(x10 & x9 & x3 & x12 | x10 & x9 & x8 | x9 & x8 & x12) & ~(x11 & x3) & ~(x11 & x8 & x12) & ~x5 & ~x7 & x1 | (x10 & x9 & x3 & x12 | x10 & x9 & x8 | x9 & x8 & x12) & ~(x11 & x3) & ~(x11 & x8) & ~x5 & ~x7 & x0 | x3 & x4 & ~x5 & ~x7 & x1 | x3 & x4 & ~x5 & ~x7 & x0 | x2 & x6 & ~x5 & ~x7 & x0");
        final Formula f4 = parser.parse("(x1 & x3 & x4 | x0 & (x2 & x6 | x3 & x4) | x9 & (x1 & x10 & x8 & ~x12 & x3 | (x1 | x0) & (x12 & (x10 & x3 | x8) | x10 & x8) & ~x11)) & ~x5 & ~x7");
        assertThat(f.not(f.equivalence(f1, f2)).cnf()).isNotEqualTo(null);
        assertThat(f.not(f.equivalence(f3, f4)).cnf()).isNotEqualTo(null);
    }

    @Test
    public void testWrongFallbackForConfig() {
        assertThatThrownBy(() -> CNFConfig.builder().fallbackAlgorithmForAdvancedEncoding(CNFConfig.Algorithm.FACTORIZATION).build()).isInstanceOf(IllegalArgumentException.class);
    }

    private boolean equivalentModels(final Formula f1, final Formula f2, final SortedSet<Variable> vars) {
        final SATSolver s = MiniSat.miniSat(f1.factory());
        s.add(f1);
        final List<Assignment> models1 = s.enumerateAllModels(vars);
        s.reset();
        s.add(f2);
        final List<Assignment> models2 = s.enumerateAllModels(vars);
        if (models1.size() != models2.size()) {
            return false;
        }
        for (final Assignment model : models1) {
            if (!models2.contains(model)) {
                return false;
            }
        }
        return true;
    }
}
