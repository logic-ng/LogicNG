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

package org.logicng.formulas;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.TestWithExampleFormulas.parse;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.predicates.NNFPredicate;
import org.logicng.predicates.satisfiability.ContingencyPredicate;
import org.logicng.predicates.satisfiability.ContradictionPredicate;
import org.logicng.predicates.satisfiability.SATPredicate;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.transformations.dnf.DNFFactorization;

import java.util.List;

public class FormulaFactoryWithoutContradictionCheckTest {

    private final FormulaFactoryConfig config = FormulaFactoryConfig.builder().simplifyComplementaryOperands(false).build();
    private final FormulaFactory f = new FormulaFactory(this.config);
    private final Variable a = this.f.variable("A");
    private final Literal notA = this.f.literal("A", false);
    private final Formula tautology = this.f.or(this.a, this.f.literal("A", false));
    private final Formula contradiction = this.f.and(this.a, this.f.literal("A", false));

    @Test
    public void testSimpleFormulas() {
        assertThat(parse(this.f, "$true").toString()).isEqualTo("$true");
        assertThat(parse(this.f, "$false").toString()).isEqualTo("$false");
        assertThat(parse(this.f, "A").toString()).isEqualTo("A");
        assertThat(parse(this.f, "~A").toString()).isEqualTo("~A");
        assertThat(parse(this.f, "A & A & B").toString()).isEqualTo("A & B");
        assertThat(parse(this.f, "A | A | B").toString()).isEqualTo("A | B");
        assertThat(parse(this.f, "A => A & B").toString()).isEqualTo("A => A & B");
        assertThat(parse(this.f, "A <=> A & B").toString()).isEqualTo("A <=> A & B");
    }

    @Test
    public void testContradictions() {
        assertThat(parse(this.f, "A & ~A").toString()).isEqualTo("A & ~A");
        assertThat(parse(this.f, "~A & A").toString()).isEqualTo("A & ~A");
        assertThat(parse(this.f, "~A & A & A & ~A & A & A & ~A").toString()).isEqualTo("A & ~A");
        assertThat(parse(this.f, "(A | B) & ~(A | B)").toString()).isEqualTo("(A | B) & ~(A | B)");
        assertThat(parse(this.f, "(A | B) & ~(B | A)").toString()).isEqualTo("(A | B) & ~(A | B)");
    }

    @Test
    public void testTautologies() {
        assertThat(parse(this.f, "A | ~A").toString()).isEqualTo("A | ~A");
        assertThat(parse(this.f, "~A | A").toString()).isEqualTo("A | ~A");
        assertThat(parse(this.f, "~A | A | A | ~A | A | A | ~A").toString()).isEqualTo("A | ~A");
        assertThat(parse(this.f, "(A & B) | ~(A & B)").toString()).isEqualTo("A & B | ~(A & B)");
        assertThat(parse(this.f, "(A & B) | ~(B & A)").toString()).isEqualTo("A & B | ~(A & B)");
    }

    @Test
    public void testFormulaProperties() {
        assertThat(this.tautology.isConstantFormula()).isFalse();
        assertThat(this.contradiction.isConstantFormula()).isFalse();
        assertThat(this.tautology.isAtomicFormula()).isFalse();
        assertThat(this.contradiction.isAtomicFormula()).isFalse();
        assertThat(this.tautology.numberOfAtoms()).isEqualTo(2);
        assertThat(this.contradiction.numberOfAtoms()).isEqualTo(2);
        assertThat(this.tautology.numberOfNodes()).isEqualTo(3);
        assertThat(this.contradiction.numberOfNodes()).isEqualTo(3);
        assertThat(this.tautology.type()).isEqualTo(FType.OR);
        assertThat(this.contradiction.type()).isEqualTo(FType.AND);
        assertThat(this.tautology.variables()).containsExactly(this.a);
        assertThat(this.contradiction.variables()).containsExactly(this.a);
        assertThat(this.tautology.literals()).containsExactlyInAnyOrder(this.a, this.notA);
        assertThat(this.contradiction.literals()).containsExactlyInAnyOrder(this.a, this.notA);
        assertThat(this.tautology.containsNode(this.a)).isTrue();
        assertThat(this.contradiction.containsNode(this.a)).isTrue();
        assertThat(this.tautology.containsNode(this.notA)).isTrue();
        assertThat(this.contradiction.containsNode(this.notA)).isTrue();
        assertThat(this.tautology.containsNode(this.tautology)).isTrue();
        assertThat(this.contradiction.containsNode(this.tautology)).isFalse();
        assertThat(this.tautology.containsNode(this.contradiction)).isFalse();
        assertThat(this.contradiction.containsNode(this.contradiction)).isTrue();
    }

    @Test
    public void testEval() {
        assertThat(this.tautology.evaluate(new Assignment())).isTrue();
        assertThat(this.tautology.evaluate(new Assignment(this.a))).isTrue();
        assertThat(this.tautology.evaluate(new Assignment(this.notA))).isTrue();
        assertThat(this.contradiction.evaluate(new Assignment())).isFalse();
        assertThat(this.contradiction.evaluate(new Assignment(this.a))).isFalse();
        assertThat(this.contradiction.evaluate(new Assignment(this.notA))).isFalse();
    }

    @Test
    public void testRestrict() {
        assertThat(this.tautology.restrict(new Assignment())).isEqualTo(this.tautology);
        assertThat(this.tautology.restrict(new Assignment(this.a))).isEqualTo(this.f.verum());
        assertThat(this.tautology.restrict(new Assignment(this.notA))).isEqualTo(this.f.verum());
        assertThat(this.contradiction.restrict(new Assignment())).isEqualTo(this.contradiction);
        assertThat(this.contradiction.restrict(new Assignment(this.a))).isEqualTo(this.f.falsum());
        assertThat(this.contradiction.restrict(new Assignment(this.notA))).isEqualTo(this.f.falsum());
    }

    @Test
    public void testNormalforms() {
        assertThat(this.tautology.nnf()).isEqualTo(this.tautology);
        assertThat(this.contradiction.nnf()).isEqualTo(this.contradiction);
        assertThat(this.tautology.cnf()).isEqualTo(this.tautology);
        assertThat(this.contradiction.cnf()).isEqualTo(this.contradiction);
        assertThat(this.tautology.transform(new DNFFactorization())).isEqualTo(this.tautology);
        assertThat(this.contradiction.transform(new DNFFactorization())).isEqualTo(this.contradiction);
    }

    @Test
    public void testPredicates() {
        assertThat(this.tautology.isCNF()).isTrue();
        assertThat(this.contradiction.isCNF()).isTrue();
        assertThat(this.tautology.holds(NNFPredicate.get())).isTrue();
        assertThat(this.contradiction.holds(NNFPredicate.get())).isTrue();
        assertThat(this.tautology.isDNF()).isTrue();
        assertThat(this.contradiction.isDNF()).isTrue();
        assertThat(this.tautology.holds(new SATPredicate(this.f))).isTrue();
        assertThat(this.contradiction.holds(new SATPredicate(this.f))).isFalse();
        assertThat(this.tautology.holds(new TautologyPredicate(this.f))).isTrue();
        assertThat(this.contradiction.holds(new TautologyPredicate(this.f))).isFalse();
        assertThat(this.tautology.holds(new ContradictionPredicate(this.f))).isFalse();
        assertThat(this.contradiction.holds(new ContradictionPredicate(this.f))).isTrue();
        assertThat(this.tautology.holds(new ContingencyPredicate(this.f))).isFalse();
        assertThat(this.contradiction.holds(new ContingencyPredicate(this.f))).isFalse();
    }

    @Test
    public void testSatSolverWithTautologies() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(parse(this.f, "A"));
        solver.add(parse(this.f, "A => B"));
        solver.add(parse(this.f, "C | ~C"));
        List<Assignment> models = solver.enumerateAllModels();
        assertThat(models).hasSize(2);
        models.forEach(m -> assertThat(m.literals()).containsAnyOf(this.f.literal("C", true), this.f.literal("C", false)));
        solver.add(parse(this.f, "D | ~D"));
        models = solver.enumerateAllModels();
        assertThat(models).hasSize(4);
        models.forEach(m -> assertThat(m.literals()).containsAnyOf(this.f.literal("C", true), this.f.literal("C", false),
                this.f.literal("D", true), this.f.literal("D", false)));
    }

    @Test
    public void testSatSolverWithContradictions() {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(parse(this.f, "A"));
        solver.add(parse(this.f, "A => B"));
        solver.add(parse(this.f, "C | ~C"));
        final List<Assignment> models = solver.enumerateAllModels();
        assertThat(models).hasSize(2);
        models.forEach(m -> assertThat(m.literals()).containsAnyOf(this.f.literal("C", true), this.f.literal("C", false)));
        solver.add(parse(this.f, "D & ~D"));
        assertThat(solver.sat()).isEqualTo(Tristate.FALSE);
    }

    @Test
    public void testSubsumption() {
        assertThat(this.tautology.substitute(this.a, this.notA)).isEqualTo(this.tautology);
        assertThat(this.contradiction.substitute(this.a, this.notA)).isEqualTo(this.contradiction);
        assertThat(this.tautology.substitute(this.a, this.f.variable("B"))).isEqualTo(parse(this.f, "B | ~B"));
        assertThat(this.contradiction.substitute(this.a, this.f.variable("B"))).isEqualTo(parse(this.f, "B & ~B"));
    }

    @Test
    public void testBdds() {
        assertThat(this.tautology.bdd().isTautology()).isTrue();
        assertThat(this.contradiction.bdd().isTautology()).isFalse();
        assertThat(this.tautology.bdd().isContradiction()).isFalse();
        assertThat(this.contradiction.bdd().isContradiction()).isTrue();
    }
}
