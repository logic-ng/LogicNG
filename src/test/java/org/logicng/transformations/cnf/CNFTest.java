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

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.handlers.FactorizationHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.predicates.CNFPredicate;
import org.logicng.predicates.DNFPredicate;

/**
 * Unit Tests for CNF conversion.
 * @version 2.0.0
 * @since 1.0
 */
public class CNFTest extends TestWithExampleFormulas {

    private final FactorizationHandler handler = new TestFactorizationHandler();
    private final CNFFactorization cnf = new CNFFactorization(this.handler);
    private final CNFPredicate cnfPredicate = CNFPredicate.get();
    private final DNFPredicate dnfPredicate = DNFPredicate.get();

    @Test
    public void testConstants() {
        assertThat(this.TRUE.transform(this.cnf)).isEqualTo(this.TRUE);
        assertThat(this.FALSE.transform(this.cnf)).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.transform(this.cnf)).isEqualTo(this.A);
        assertThat(this.NA.transform(this.cnf)).isEqualTo(this.NA);
    }

    @Test
    public void testBinaryOperators() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.IMP1.transform(this.cnf)).isEqualTo(p.parse("~a | b"));
        assertThat(this.IMP2.transform(this.cnf)).isEqualTo(p.parse("a | ~b"));
        assertThat(this.IMP3.transform(this.cnf)).isEqualTo(p.parse("~a | ~b | x | y"));
        assertThat(this.EQ1.transform(this.cnf)).isEqualTo(p.parse("(a | ~b) & (~a | b)"));
        assertThat(this.EQ2.transform(this.cnf)).isEqualTo(p.parse("(~a | b) & (a | ~b)"));
        assertThat(this.IMP1.transform(this.cnf).holds(this.cnfPredicate)).isTrue();
        assertThat(this.IMP2.transform(this.cnf).holds(this.cnfPredicate)).isTrue();
        assertThat(this.IMP3.transform(this.cnf).holds(this.cnfPredicate)).isTrue();
        assertThat(this.EQ1.transform(this.cnf).holds(this.cnfPredicate)).isTrue();
        assertThat(this.EQ1.transform(this.cnf).holds(this.dnfPredicate)).isFalse();
        assertThat(this.EQ2.transform(this.cnf).holds(this.cnfPredicate)).isTrue();
        assertThat(this.EQ2.transform(this.cnf).holds(this.dnfPredicate)).isFalse();
    }

    @Test
    public void testNAryOperators() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.AND1.transform(this.cnf)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.cnf)).isEqualTo(this.OR1);
        assertThat(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(this.cnf)).isEqualTo(p.parse("~a & ~b & c & (~x | y) & (~w | z)"));
        assertThat(p.parse("~(a & b) | c | ~(x | ~y)").transform(this.cnf)).isEqualTo(p.parse("(~a | ~b | c | ~x) & (~a  | ~b | c | y)"));
        assertThat(p.parse("a | b | (~x & ~y)").transform(this.cnf)).isEqualTo(p.parse("(a | b | ~x) & (a | b | ~y)"));
        assertThat(this.AND1.transform(this.cnf).holds(this.cnfPredicate)).isTrue();
        assertThat(this.OR1.transform(this.cnf).holds(this.cnfPredicate)).isTrue();
        assertThat(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(this.cnf).holds(this.cnfPredicate)).isTrue();
        assertThat(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(this.cnf).holds(this.dnfPredicate)).isFalse();
        assertThat(p.parse("~(a & b) | c | ~(x | ~y)").transform(this.cnf).holds(this.cnfPredicate)).isTrue();
        assertThat(p.parse("~(a & b) | c | ~(x | ~y)").transform(this.cnf).holds(this.dnfPredicate)).isFalse();
        assertThat(p.parse("a | b | (~x & ~y)").transform(this.cnf).holds(this.cnfPredicate)).isTrue();
        assertThat(p.parse("a | b | (~x & ~y)").transform(this.cnf).holds(this.dnfPredicate)).isFalse();
    }

    @Test
    public void testNot() throws ParserException {
        final TestFactorizationHandler handler2 = new TestFactorizationHandler();
        final CNFFactorization cnf2 = new CNFFactorization(handler2);
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(p.parse("~a2").transform(this.cnf)).isEqualTo(p.parse("~a2"));
        assertThat(p.parse("~~a2").transform(this.cnf)).isEqualTo(p.parse("a2"));
        assertThat(p.parse("~(a2 => b2)").transform(this.cnf)).isEqualTo(p.parse("a2 & ~b2"));
        assertThat(p.parse("~(~(a2 | b2) => ~(x2 | y2))").transform(this.cnf)).isEqualTo(p.parse("~a2 & ~b2 & (x2 | y2)"));
        assertThat(p.parse("~(a2 <=> b2)").transform(this.cnf)).isEqualTo(p.parse("(~a2 | ~b2) & (a2 | b2)"));
        assertThat(p.parse("~(~(a2 | b2) <=> ~(x2 | y2))").transform(cnf2)).isEqualTo(p.parse("(a2 | b2 | x2 | y2) & (~a2 | ~x2) & (~a2 | ~y2) & (~b2 | ~x2) & (~b2 | ~y2)"));
        assertThat(p.parse("~(a2 & b2 & ~x2 & ~y2)").transform(this.cnf)).isEqualTo(p.parse("~a2 | ~b2 | x2 | y2"));
        assertThat(p.parse("~(a2 | b2 | ~x2 | ~y2)").transform(this.cnf)).isEqualTo(p.parse("~a2 & ~b2 & x2 & y2"));
        assertThat(p.parse("~(a2 | b2 | ~x2 | ~y2)").transform(this.cnf)).isEqualTo(p.parse("~a2 & ~b2 & x2 & y2"));
        assertThat(handler2.distCount).isEqualTo(7);
        assertThat(handler2.clauseCount).isEqualTo(4);
        assertThat(handler2.longestClause).isEqualTo(2);
    }

    @Test
    public void testCC() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        assertThat(p.parse("a <=> (1 * b <= 1)").cnf()).isEqualTo(p.parse("a"));
        assertThat(p.parse("~(1 * b <= 1)").cnf()).isEqualTo(p.parse("$false"));
        assertThat(p.parse("(1 * b + 1 * c + 1 * d <= 1)").cnf()).isEqualTo(p.parse("(~b | ~c) & (~b | ~d) & (~c | ~d)"));
        assertThat(p.parse("~(1 * b + 1 * c + 1 * d <= 1)").cnf()).isEqualTo(p.parse("(d | @RESERVED_CC_1 | @RESERVED_CC_4) & (~@RESERVED_CC_3 | @RESERVED_CC_1 | @RESERVED_CC_4) & (~@RESERVED_CC_3 | d | @RESERVED_CC_4) & (~@RESERVED_CC_4 | @RESERVED_CC_0) & (~@RESERVED_CC_2 | @RESERVED_CC_0) & (~@RESERVED_CC_4 | ~@RESERVED_CC_2) & (c | @RESERVED_CC_3 | @RESERVED_CC_5) & (b | @RESERVED_CC_3 | @RESERVED_CC_5) & (b | c | @RESERVED_CC_5) & (~@RESERVED_CC_5 | @RESERVED_CC_2) & ~@RESERVED_CC_0"));
    }

    @Test
    public void testToString() {
        assertThat(this.cnf.toString()).isEqualTo("CNFFactorization");
    }

    private static class TestFactorizationHandler implements FactorizationHandler {

        private boolean aborted;
        private int distCount = 0;
        private int clauseCount = 0;
        private long longestClause = 0;

        @Override
        public boolean aborted() {
            return this.aborted;
        }

        @Override
        public void started() {
            this.aborted = false;
            this.distCount = 0;
            this.clauseCount = 0;
            this.longestClause = 0;
        }

        @Override
        public boolean performedDistribution() {
            this.distCount++;
            return true;
        }

        @Override
        public boolean createdClause(final Formula clause) {
            this.clauseCount++;
            this.longestClause = Math.max(clause.numberOfAtoms(), this.longestClause);
            return true;
        }
    }

    @Test
    public void testWithHandler() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        Formula formula = p.parse("(~(~(a | b) => ~(x | y))) & ((a | x) => ~(b | y))");
        final FactorizationHandler handler = new FactorizationHandler() {
            private boolean aborted;
            private int dists = 0;
            private int clauses = 0;

            @Override
            public boolean aborted() {
                return this.aborted;
            }

            @Override
            public void started() {
                this.aborted = false;
                this.dists = 0;
                this.clauses = 0;
            }

            @Override
            public boolean performedDistribution() {
                this.dists++;
                this.aborted = this.dists >= 100;
                return !this.aborted;
            }

            @Override
            public boolean createdClause(final Formula clause) {
                this.clauses++;
                this.aborted = this.clauses >= 2;
                return !this.aborted;
            }
        };
        final CNFFactorization factorization = new CNFFactorization(handler);
        Formula cnf = factorization.apply(formula, false);
        assertThat(handler.aborted()).isTrue();
        assertThat(cnf).isNull();

        formula = p.parse("~(a | b)");
        cnf = factorization.apply(formula, false);
        assertThat(handler.aborted()).isFalse();
        assertThat(cnf).isNotNull();
    }
}
