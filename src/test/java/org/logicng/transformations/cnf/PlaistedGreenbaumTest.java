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
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.predicates.CNFPredicate;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.List;
import java.util.SortedSet;

/**
 * Unit Tests for {@link PlaistedGreenbaumTransformation}.
 * @version 2.0.0
 * @since 1.0
 */
public class PlaistedGreenbaumTest extends TestWithExampleFormulas {

    private final PlaistedGreenbaumTransformation pg = new PlaistedGreenbaumTransformation(0);
    private final CNFPredicate cnfPredicate = CNFPredicate.get();

    @Test
    public void testConstants() {
        assertThat(this.TRUE.transform(this.pg)).isEqualTo(this.TRUE);
        assertThat(this.FALSE.transform(this.pg)).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.transform(this.pg)).isEqualTo(this.A);
        assertThat(this.NA.transform(this.pg)).isEqualTo(this.NA);
    }

    @Test
    public void testBinaryOperators() {
        assertThat(this.IMP1.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.IMP1, this.IMP1.transform(this.pg), this.IMP1.variables())).isTrue();
        assertThat(this.IMP2.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.IMP2, this.IMP2.transform(this.pg), this.IMP2.variables())).isTrue();
        assertThat(this.IMP3.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.IMP3, this.IMP3.transform(this.pg), this.IMP3.variables())).isTrue();
        assertThat(this.EQ1.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ1, this.EQ1.transform(this.pg), this.EQ1.variables())).isTrue();
        assertThat(this.EQ2.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ2, this.EQ2.transform(this.pg), this.EQ2.variables())).isTrue();
        assertThat(this.EQ3.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ3, this.EQ3.transform(this.pg), this.EQ3.variables())).isTrue();
        assertThat(this.EQ4.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ4, this.EQ4.transform(this.pg), this.EQ4.variables())).isTrue();
    }

    @Test
    public void testNAryOperators() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(this.AND1.transform(this.pg)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.pg)).isEqualTo(this.OR1);
        final Formula f1 = p.parse("(a & b & x) | (c & d & ~y)");
        final Formula f2 = p.parse("(a & b & x) | (c & d & ~y) | (~z | (c & d & ~y)) ");
        final Formula f3 = p.parse("a | b | (~x & ~y)");
        assertThat(f1.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.pg), f1.variables())).isTrue();
        assertThat(f2.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.pg), f2.variables())).isTrue();
        assertThat(f3.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.pg), f3.variables())).isTrue();
    }

    @Test
    public void testNotNary() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(p.parse("~a").transform(this.pg)).isEqualTo(p.parse("~a"));
        assertThat(p.parse("~~a").transform(this.pg)).isEqualTo(p.parse("a"));
        final Formula f0 = p.parse("~(~a | b)");
        final Formula f1 = p.parse("~((a | b) | ~(x | y))");
        final Formula f2 = p.parse("~(a & b | ~a & ~b)");
        final Formula f3 = p.parse("~(~(a | b) & ~(x | y) | (a | b) & (x | y))");
        final Formula f4 = p.parse("~(a & b & ~x & ~y)");
        final Formula f5 = p.parse("~(a | b | ~x | ~y)");
        final Formula f6 = p.parse("~(a & b) & (c | (a & b))");
        assertThat(f0.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f0, f0.transform(this.pg), f0.variables())).isTrue();
        assertThat(f1.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.pg), f1.variables())).isTrue();
        assertThat(f2.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.pg), f2.variables())).isTrue();
        assertThat(f3.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.pg), f3.variables())).isTrue();
        assertThat(f4.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f4, f4.transform(this.pg), f4.variables())).isTrue();
        assertThat(f5.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f5, f5.transform(this.pg), f5.variables())).isTrue();
        assertThat(f5.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f5, f5.transform(this.pg), f5.variables())).isTrue();
        assertThat(f6.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f6, f6.transform(this.pg), f6.variables())).isTrue();
    }

    @Test
    public void testNotBinary() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(p.parse("~a").transform(this.pg)).isEqualTo(p.parse("~a"));
        assertThat(p.parse("~~a").transform(this.pg)).isEqualTo(p.parse("a"));
        final Formula f1 = p.parse("~(~(a | b) => ~(x | y))");
        final Formula f2 = p.parse("~(a <=> b)");
        final Formula f3 = p.parse("~(~(a | b) <=> ~(x | y))");
        assertThat(f1.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.pg), f1.variables())).isTrue();
        assertThat(f2.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.pg), f2.variables())).isTrue();
        assertThat(f3.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.pg), f3.variables())).isTrue();
    }

    @Test
    public void testCC() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        assertThat(p.parse("a <=> (1 * b <= 1)").transform(this.pg)).isEqualTo(p.parse("a"));
        assertThat(p.parse("~(1 * b <= 1)").transform(this.pg)).isEqualTo(p.parse("$false"));
        assertThat(p.parse("(1 * b + 1 * c + 1 * d <= 1)").transform(this.pg)).isEqualTo(p.parse("(~b | ~c) & (~b | ~d) & (~c | ~d)"));
        assertThat(p.parse("~(1 * b + 1 * c + 1 * d <= 1)").transform(this.pg)).isEqualTo(p.parse("(d | @RESERVED_CC_1 | @RESERVED_CC_4) & (~@RESERVED_CC_3 | @RESERVED_CC_1 | @RESERVED_CC_4) & (~@RESERVED_CC_3 | d | @RESERVED_CC_4) & (~@RESERVED_CC_4 | @RESERVED_CC_0) & (~@RESERVED_CC_2 | @RESERVED_CC_0) & (~@RESERVED_CC_4 | ~@RESERVED_CC_2) & (c | @RESERVED_CC_3 | @RESERVED_CC_5) & (b | @RESERVED_CC_3 | @RESERVED_CC_5) & (b | c | @RESERVED_CC_5) & (~@RESERVED_CC_5 | @RESERVED_CC_2) & ~@RESERVED_CC_0"));
    }

    @Test
    public void testFormulas() throws ParserException {
        final FormulaFactory fac = new FormulaFactory();
        final PlaistedGreenbaumTransformation pgNNF = new PlaistedGreenbaumTransformation(0);
        final PropositionalParser p = new PropositionalParser(fac);
        final Formula f1 = p.parse("(a | b) => c");
        final Formula f2 = p.parse("~x & ~y");
        final Formula f3 = p.parse("d & ((a | b) => c)");
        final Formula f4 = p.parse("d & ((a | b) => c) | ~x & ~y");
        assertThat(f1.transform(pgNNF)).isEqualTo(p.parse("(@RESERVED_CNF_1 | c) & (~@RESERVED_CNF_1 | ~a) & (~@RESERVED_CNF_1 | ~b)"));
        assertThat(f2.transform(pgNNF)).isEqualTo(p.parse("~x & ~y"));
        assertThat(f3.transform(pgNNF)).isEqualTo(p.parse("d & @RESERVED_CNF_0 & (~@RESERVED_CNF_0 | @RESERVED_CNF_1 | c) & " +
                "(~@RESERVED_CNF_1 | ~a) & (~@RESERVED_CNF_1 | ~b)"));
        assertThat(f4.transform(pgNNF)).isEqualTo(p.parse("(@RESERVED_CNF_2 | @RESERVED_CNF_4) & (~@RESERVED_CNF_2 | d) & " +
                "(~@RESERVED_CNF_2 | @RESERVED_CNF_0) & (~@RESERVED_CNF_0 | @RESERVED_CNF_1 | c) & " +
                "(~@RESERVED_CNF_1 | ~a) & (~@RESERVED_CNF_1 | ~b) & (~@RESERVED_CNF_4 | ~x) & " +
                "(~@RESERVED_CNF_4 | ~y)"));
        assertThat(f1.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.pg), f1.variables())).isTrue();
        assertThat(f2.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.pg), f2.variables())).isTrue();
        assertThat(f3.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.pg), f3.variables())).isTrue();
        assertThat(f4.transform(this.pg, false).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f4, f4.transform(this.pg, false), f4.variables())).isTrue();
        assertThat(f4.transform(this.pg).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f4, f4.transform(this.pg), f4.variables())).isTrue();
    }

    @Test
    public void testFactorization() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        final PlaistedGreenbaumTransformation pgf = new PlaistedGreenbaumTransformation();
        final Formula f1 = p.parse("(a | b) => c");
        final Formula f2 = p.parse("~x & ~y");
        final Formula f3 = p.parse("d & ((a | b) => c)");
        final Formula f4 = p.parse("d & ((a | b) => c) | ~x & ~y");
        assertThat(f1.transform(pgf).holds(this.cnfPredicate)).isTrue();
        assertThat(f1.transform(pgf).variables().size()).isEqualTo(f1.variables().size());
        assertThat(f2.transform(pgf).holds(this.cnfPredicate)).isTrue();
        assertThat(f2.transform(pgf).variables().size()).isEqualTo(f2.variables().size());
        assertThat(f3.transform(pgf).holds(this.cnfPredicate)).isTrue();
        assertThat(f3.transform(pgf).variables().size()).isEqualTo(f3.variables().size());
        assertThat(f4.transform(pgf).holds(this.cnfPredicate)).isTrue();
        assertThat(f4.transform(pgf).variables().size()).isEqualTo(f4.variables().size());
    }

    @Test
    public void testToString() {
        final PlaistedGreenbaumTransformation pGTransformation = new PlaistedGreenbaumTransformation(5);
        assertThat(pGTransformation.toString()).isEqualTo("PlaistedGreenbaumTransformation{boundary=5}");
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
