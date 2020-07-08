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
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.predicates.CNFPredicate;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.List;
import java.util.SortedSet;

/**
 * Unit Tests for {@link BDDCNFTransformation}.
 * @version 2.0.0
 * @since 1.4.0
 */
public class BDDCNFTest extends TestWithExampleFormulas {

    private final BDDCNFTransformation bddcnf = new BDDCNFTransformation();
    private final CNFPredicate cnfPredicate = CNFPredicate.get();

    @Test
    public void testConstants() {
        assertThat(this.TRUE.transform(this.bddcnf)).isEqualTo(this.TRUE);
        assertThat(this.FALSE.transform(this.bddcnf)).isEqualTo(this.FALSE);
    }

    @Test
    public void testLiterals() {
        assertThat(this.A.transform(this.bddcnf)).isEqualTo(this.A);
        assertThat(this.NA.transform(this.bddcnf)).isEqualTo(this.NA);
    }

    @Test
    public void testBinaryOperators() {
        assertThat(this.IMP1.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.IMP1, this.IMP1.transform(this.bddcnf), this.IMP1.variables())).isTrue();
        assertThat(this.IMP2.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.IMP2, this.IMP2.transform(this.bddcnf), this.IMP2.variables())).isTrue();
        assertThat(this.IMP3.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.IMP3, this.IMP3.transform(this.bddcnf), this.IMP3.variables())).isTrue();
        assertThat(this.EQ1.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ1, this.EQ1.transform(this.bddcnf), this.EQ1.variables())).isTrue();
        assertThat(this.EQ2.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ2, this.EQ2.transform(this.bddcnf), this.EQ2.variables())).isTrue();
        assertThat(this.EQ3.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ3, this.EQ3.transform(this.bddcnf), this.EQ3.variables())).isTrue();
        assertThat(this.EQ4.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(this.EQ4, this.EQ4.transform(this.bddcnf), this.EQ4.variables())).isTrue();
    }

    @Test
    public void testNAryOperators() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        assertThat(this.AND1.transform(this.bddcnf)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.bddcnf)).isEqualTo(this.OR1);
        final Formula f1 = p.parse("~(a | b) & c & ~(x & ~y) & (w => z)");
        final Formula f2 = p.parse("~(a & b) | c | ~(x | ~y)");
        final Formula f3 = p.parse("a | b | (~x & ~y)");
        assertThat(f1.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.bddcnf), f1.variables())).isTrue();
        assertThat(f2.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.bddcnf), f2.variables())).isTrue();
        assertThat(f3.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.bddcnf), f3.variables())).isTrue();
    }

    @Test
    public void testNAryOperatorsWithExternalFactory() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final BDDCNFTransformation transformation = new BDDCNFTransformation(f, 7);
        assertThat(this.AND1.transform(this.bddcnf)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.bddcnf)).isEqualTo(this.OR1);
        final Formula f1 = p.parse("~(a | b) & c & ~(x & ~y) & (w => z)");
        final Formula f2 = p.parse("~(a & b) | c | ~(x | ~y)");
        final Formula f3 = p.parse("a | b | (~x & ~y)");
        assertThat(f1.transform(transformation).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(transformation), f1.variables())).isTrue();
        assertThat(f2.transform(transformation).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(transformation), f2.variables())).isTrue();
        assertThat(f3.transform(transformation).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(transformation), f3.variables())).isTrue();
    }

    @Test
    public void testNAryOperatorsWithExternalFactory2() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final BDDCNFTransformation transformation = new BDDCNFTransformation(new BDDKernel(f, 7, 50, 50));
        assertThat(this.AND1.transform(this.bddcnf)).isEqualTo(this.AND1);
        assertThat(this.OR1.transform(this.bddcnf)).isEqualTo(this.OR1);
        final Formula f1 = p.parse("~(a | b) & c & ~(x & ~y) & (w => z)");
        final Formula f2 = p.parse("~(a & b) | c | ~(x | ~y)");
        final Formula f3 = p.parse("a | b | (~x & ~y)");
        assertThat(f1.transform(transformation).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(transformation), f1.variables())).isTrue();
        assertThat(f2.transform(transformation).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(transformation), f2.variables())).isTrue();
        assertThat(f3.transform(transformation).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(transformation), f3.variables())).isTrue();
    }

    @Test
    public void testNot() throws ParserException {
        final PropositionalParser p = new PropositionalParser(this.f);
        assertThat(p.parse("~a").transform(this.bddcnf)).isEqualTo(p.parse("~a"));
        assertThat(p.parse("~~a").transform(this.bddcnf)).isEqualTo(p.parse("a"));
        assertThat(p.parse("~(a => b)").transform(this.bddcnf)).isEqualTo(p.parse("a & ~b"));
        final Formula f1 = p.parse("~(~(a | b) => ~(x | y))");
        final Formula f2 = p.parse("~(a <=> b)");
        final Formula f3 = p.parse("~(~(a | b) <=> ~(x | y))");
        final Formula f4 = p.parse("~(a & b & ~x & ~y)");
        final Formula f5 = p.parse("~(a | b | ~x | ~y)");
        assertThat(f1.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.bddcnf), f1.variables())).isTrue();
        assertThat(f2.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.bddcnf), f2.variables())).isTrue();
        assertThat(f3.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.bddcnf), f3.variables())).isTrue();
        assertThat(f4.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f4, f4.transform(this.bddcnf), f4.variables())).isTrue();
        assertThat(f5.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f5, f5.transform(this.bddcnf), f5.variables())).isTrue();
        assertThat(f5.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f5, f5.transform(this.bddcnf), f5.variables())).isTrue();
    }

    @Test
    public void testCC() throws ParserException {
        final PseudoBooleanParser p = new PseudoBooleanParser(this.f);
        final Formula f1 = p.parse("a <=> (1 * b <= 1)");
        final Formula f2 = p.parse("~(1 * b <= 1)");
        final Formula f3 = p.parse("(1 * b + 1 * c + 1 * d <= 1)");
        final Formula f4 = p.parse("~(1 * b + 1 * c + 1 * d <= 1)");
        assertThat(f1.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f1, f1.transform(this.bddcnf), f1.variables())).isTrue();
        assertThat(f2.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f2, f2.transform(this.bddcnf), f2.variables())).isTrue();
        assertThat(f3.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f3, f3.transform(this.bddcnf), f3.variables())).isTrue();
        assertThat(f4.transform(this.bddcnf).holds(this.cnfPredicate)).isTrue();
        assertThat(equivalentModels(f4, f4.transform(this.bddcnf), f4.variables())).isTrue();
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
