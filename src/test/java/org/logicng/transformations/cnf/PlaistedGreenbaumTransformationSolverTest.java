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
import org.logicng.RandomTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.predicates.satisfiability.SATPredicate;
import org.logicng.predicates.satisfiability.TautologyPredicate;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.util.FormulaCornerCases;
import org.logicng.util.FormulaRandomizer;
import org.logicng.util.FormulaRandomizerConfig;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Unit Tests for the class {@link PlaistedGreenbaumTransformationSolver}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class PlaistedGreenbaumTransformationSolverTest extends TestWithExampleFormulas {

    @Test
    public void testCornerCases() {
        final FormulaFactory f = new FormulaFactory();
        final FormulaCornerCases cornerCases = new FormulaCornerCases(f);
        for (final Formula formula : cornerCases.cornerCases()) {
            final SATSolver solverFactorization = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).build());
            final SATSolver solverFullPG = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FULL_PG_ON_SOLVER).build());
            solverFactorization.add(formula);
            solverFullPG.add(formula);
            assertThat(solverFactorization.sat() == solverFullPG.sat()).isTrue();
        }
    }

    @Test
    @RandomTag
    public void random() {
        for (int i = 0; i < 1000; i++) {
            final FormulaFactory f = new FormulaFactory();
            final SATSolver solver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FULL_PG_ON_SOLVER).auxiliaryVariablesInModels(false).build());
            final FormulaRandomizer randomizer = new FormulaRandomizer(f, FormulaRandomizerConfig.builder().numVars(10).weightPbc(1).seed(i * 42).build());

            final Formula randomFormula01 = randomSATFormula(randomizer, 4, f);
            final Formula randomFormula02 = randomSATFormula(randomizer, 4, f);
            solver.reset();
            solver.add(randomFormula01);
            if (solver.sat() == Tristate.TRUE) {
                final List<Assignment> models = solver.enumerateAllModels();
                final Formula dnf = f.or(models.stream().map(model -> f.and(model.literals())).collect(Collectors.toList()));
                assertThat(f.equivalence(randomFormula01, dnf).holds(new TautologyPredicate(f))).isTrue();
            }
            final SolverState state = solver.saveState();
            solver.add(randomFormula02);
            if (solver.sat() == Tristate.TRUE) {
                final List<Assignment> models = solver.enumerateAllModels();
                final Formula dnf = f.or(models.stream().map(model -> f.and(model.literals())).collect(Collectors.toList()));
                assertThat(f.equivalence(f.and(randomFormula01, randomFormula02), dnf).holds(new TautologyPredicate(f))).isTrue();
            }
            solver.loadState(state);
            if (solver.sat() == Tristate.TRUE) {
                final List<Assignment> models = solver.enumerateAllModels();
                final Formula dnf = f.or(models.stream().map(model -> f.and(model.literals())).collect(Collectors.toList()));
                assertThat(f.equivalence(randomFormula01, dnf).holds(new TautologyPredicate(f))).isTrue();
            }
            solver.add(randomFormula02);
            if (solver.sat() == Tristate.TRUE) {
                final List<Assignment> models = solver.enumerateAllModels();
                final Formula dnf = f.or(models.stream().map(model -> f.and(model.literals())).collect(Collectors.toList()));
                assertThat(f.equivalence(f.and(randomFormula01, randomFormula02), dnf).holds(new TautologyPredicate(f))).isTrue();
            }
        }
    }

    private static Formula randomSATFormula(final FormulaRandomizer randomizer, final int maxDepth, final FormulaFactory f) {
        return Stream.generate(() -> randomizer.formula(maxDepth))
                .filter(formula -> formula.holds(new SATPredicate(f)))
                .findAny().get();
    }

    @Test
    public void simple() throws ParserException {
        computeAndVerify(this.f.parse("(X => (A & B)) | ~(A & B)"));
        computeAndVerify(this.f.parse("~((A | (C & D)) & ((X & ~Z) | (H & E)))"));

        computeAndVerify(this.f.parse("A | (B | ~C) & (D | ~E)"));
        computeAndVerify(this.f.parse("A | B & (C | D)"));

        computeAndVerify(this.f.parse("~(A&B)|X"));
        computeAndVerify(this.f.parse("~(~(A&B)|X)"));

        computeAndVerify(this.f.parse("~(~(A&B)|X)"));
        computeAndVerify(this.f.parse("~(A&B=>X)"));

        computeAndVerify(this.f.parse("A&B => X"));
        computeAndVerify(this.f.parse("~(A&B=>X)"));

        computeAndVerify(this.f.parse("A&B <=> X"));
        computeAndVerify(this.f.parse("~(A&B<=>X)"));

        computeAndVerify(this.f.parse("~(A&B)"));

        computeAndVerify(this.f.parse("A & (B | A => (A <=> ~B))"));
        computeAndVerify(this.f.parse("(A => ~A) <=> (B <=> (~A => B))"));
    }

    private static void computeAndVerify(final Formula formula) {
        final FormulaFactory f = formula.factory();
        final SATSolver solver = MiniSat.miniSat(f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FULL_PG_ON_SOLVER).auxiliaryVariablesInModels(false).build());
        solver.add(formula);
        final List<Assignment> models = solver.enumerateAllModels();
        final Formula dnf = f.or(models.stream().map(model -> f.and(model.literals())).collect(Collectors.toList()));
        assertThat(f.equivalence(formula, dnf).holds(new TautologyPredicate(f))).isTrue();
    }
}
