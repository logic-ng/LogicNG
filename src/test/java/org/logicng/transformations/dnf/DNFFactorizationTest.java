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

package org.logicng.transformations.dnf;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.TestWithExampleFormulas;
import org.logicng.formulas.Formula;
import org.logicng.handlers.FactorizationHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.predicates.CNFPredicate;
import org.logicng.predicates.DNFPredicate;

/**
 * Unit Tests for DNF conversion.
 * @version 2.0.0
 * @since 1.0
 */
public class DNFFactorizationTest extends TestWithExampleFormulas {

    private final DNFFactorization dnfFactorization = new DNFFactorization();
    private final DNFPredicate dnfPredicate = DNFPredicate.get();
    private final CNFPredicate cnfPredicate = CNFPredicate.get();

    @Test
    public void testConstants() {
        assertThat(this.TRUE.transform(this.dnfFactorization)).isEqualTo(this.TRUE);
        assertThat(this.FALSE.transform(this.dnfFactorization)).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.transform(this.dnfFactorization)).isEqualTo(this.A);
        assertThat(this.NA.transform(this.dnfFactorization)).isEqualTo(this.NA);
    }

    @Test
    public void testBinaryOperators() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.IMP1.transform(this.dnfFactorization)).isEqualTo(p.parse("~a | b"));
        assertThat(this.IMP2.transform(this.dnfFactorization)).isEqualTo(p.parse("a | ~b"));
        assertThat(this.IMP3.transform(this.dnfFactorization)).isEqualTo(p.parse("~a | ~b | x | y"));
        assertThat(this.EQ1.transform(this.dnfFactorization)).isEqualTo(p.parse("(a & b) | (~a & ~b)"));
        assertThat(this.EQ2.transform(this.dnfFactorization)).isEqualTo(p.parse("(a & b) | (~a & ~b)"));
        assertThat(this.IMP1.transform(this.dnfFactorization).holds(this.dnfPredicate)).isTrue();
        assertThat(this.IMP2.transform(this.dnfFactorization).holds(this.dnfPredicate)).isTrue();
        assertThat(this.IMP3.transform(this.dnfFactorization).holds(this.dnfPredicate)).isTrue();
        assertThat(this.EQ1.transform(this.dnfFactorization).holds(this.dnfPredicate)).isTrue();
        assertThat(this.EQ1.transform(this.dnfFactorization).holds(this.cnfPredicate)).isFalse();
        assertThat(this.EQ2.transform(this.dnfFactorization).holds(this.dnfPredicate)).isTrue();
        assertThat(this.EQ2.transform(this.dnfFactorization).holds(this.cnfPredicate)).isFalse();
    }

    @Test
    public void testNAryOperators() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.AND1.transform(this.dnfFactorization)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.dnfFactorization)).isEqualTo(this.OR1);
        assertThat(p.parse("~(a & b) | c | ~(x | ~y)").transform(this.dnfFactorization)).isEqualTo(p.parse("~a | ~b | c | (~x & y)"));
        assertThat(p.parse("~(a | b) & c & ~(x & ~y)").transform(this.dnfFactorization)).isEqualTo(p.parse("(~a & ~b & c & ~x) | (~a & ~b & c & y)"));
        assertThat(p.parse("a & b & (~x | ~y)").transform(this.dnfFactorization)).isEqualTo(p.parse("(a & b & ~x) | (a & b & ~y)"));
        assertThat(this.AND1.transform(this.dnfFactorization).holds(this.dnfPredicate)).isTrue();
        assertThat(this.OR1.transform(this.dnfFactorization).holds(this.dnfPredicate)).isTrue();
        assertThat(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(this.dnfFactorization).holds(this.dnfPredicate)).isTrue();
        assertThat(p.parse("~(a | b) & c & ~(x & ~y) & (w => z)").transform(this.dnfFactorization).holds(this.cnfPredicate)).isFalse();
        assertThat(p.parse("~(a & b) | c | ~(x | ~y)").transform(this.dnfFactorization).holds(this.dnfPredicate)).isTrue();
        assertThat(p.parse("~(a & b) | c | ~(x | ~y)").transform(this.dnfFactorization).holds(this.cnfPredicate)).isFalse();
        assertThat(p.parse("a | b | (~x & ~y)").transform(this.dnfFactorization).holds(this.dnfPredicate)).isTrue();
        assertThat(p.parse("a | b | (~x & ~y)").transform(this.dnfFactorization).holds(this.cnfPredicate)).isFalse();
    }

    @Test
    public void testNot() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(p.parse("~a").transform(this.dnfFactorization)).isEqualTo(p.parse("~a"));
        assertThat(p.parse("~~a").transform(this.dnfFactorization)).isEqualTo(p.parse("a"));
        assertThat(p.parse("~(a => b)").transform(this.dnfFactorization)).isEqualTo(p.parse("a & ~b"));
        assertThat(p.parse("~(~(a | b) => ~(x | y))").transform(this.dnfFactorization)).isEqualTo(p.parse("(~a & ~b & x) | (~a & ~b & y)"));
        assertThat(p.parse("~(a <=> b)").transform(this.dnfFactorization)).isEqualTo(p.parse("(~a & b) | (a & ~b)"));
        assertThat(p.parse("~(a & b & ~x & ~y)").transform(this.dnfFactorization)).isEqualTo(p.parse("~a | ~b | x | y"));
        assertThat(p.parse("~(a | b | ~x | ~y)").transform(this.dnfFactorization)).isEqualTo(p.parse("~a & ~b & x & y"));
        assertThat(p.parse("~(a | b | ~x | ~y)").transform(this.dnfFactorization)).isEqualTo(p.parse("~a & ~b & x & y"));
    }

    @Test
    public void testCDNF() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        final Formula formula = p.parse("x0 & x1 & x3 | ~x1 & ~x2 | x2 & ~x3");
        final Formula cdnf = p.parse("x0 & x1 & x2 & x3 | x0 & x1 & x2 & ~x3 | x0 & ~x1 & x2 & ~x3 | ~x0 & ~x1 & x2 & ~x3 | ~x0 & ~x1 & ~x2 & ~x3 | x0 & ~x1 & ~x2 & ~x3 | x0 & ~x1 & ~x2 & x3 | x0 & x1 & ~x2 & x3 | ~x0 & x1 & x2 & ~x3 | ~x0 & ~x1 & ~x2 & x3");
        assertThat(formula.transform(new CanonicalDNFEnumeration())).isEqualTo(cdnf);
        assertThat(this.f.and(this.A, this.NA).transform(new CanonicalDNFEnumeration())).isEqualTo(this.f.falsum());
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
                this.aborted = this.clauses >= 5;
                return !this.aborted;
            }
        };
        final DNFFactorization factorization = new DNFFactorization(handler);
        Formula dnf = factorization.apply(formula, false);
        assertThat(handler.aborted()).isTrue();
        assertThat(dnf).isNull();

        formula = p.parse("~(a | b)");
        dnf = factorization.apply(formula, false);
        assertThat(handler.aborted()).isFalse();
        assertThat(dnf).isNotNull();
    }

    @Test
    public void testToString() {
        assertThat(this.dnfFactorization.toString()).isEqualTo("DNFFactorization");
        assertThat(new CanonicalDNFEnumeration().toString()).isEqualTo("CanonicalDNFEnumeration");
    }
}
