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

package org.logicng.solvers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.parsers.ParserException;
import org.logicng.solvers.functions.ModelEnumerationFunction;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSatConfig;
import org.logicng.transformations.cnf.TseitinTransformation;
import org.logicng.util.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Test model generation and model enumeration on solvers.
 * @version 2.0.0
 * @since 1.6.0
 */
public class ModelTest {

    private static final FormulaFactory f = new FormulaFactory();

    public static Collection<Object[]> solvers() {
        final MiniSatConfig configNoPGAux = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).auxiliaryVariablesInModels(true).build();
        final MiniSatConfig configNoPGNoAux = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FACTORY_CNF).auxiliaryVariablesInModels(false).build();
        final MiniSatConfig configPGAux = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).auxiliaryVariablesInModels(true).build();
        final MiniSatConfig configPGNoAux = MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).auxiliaryVariablesInModels(false).build();
        final List<Pair<MiniSatConfig, String>> configs = Arrays.asList(
                new Pair<>(configNoPGAux, "FF CNF, +AUX"),
                new Pair<>(configNoPGNoAux, "FF CNF, -AUX"),
                new Pair<>(configPGAux, "PG CNF, +AUX"),
                new Pair<>(configPGNoAux, "PG CNF, -AUX")
        );
        final List<Object[]> solvers = new ArrayList<>();
        for (final Pair<MiniSatConfig, String> config : configs) {
            solvers.add(new Object[]{MiniSat.miniSat(f, config.first()), "MiniSat (" + config.second() + ")"});
            solvers.add(new Object[]{MiniSat.miniCard(f, config.first()), "MiniCard (" + config.second() + ")"});
            solvers.add(new Object[]{MiniSat.glucose(f, config.first(), GlucoseConfig.builder().build()), "Glucose (" + config.second() + ")"});
        }
        return solvers;
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testNoModel(final SATSolver solver) throws ParserException {
        solver.reset();
        solver.add(f.falsum());
        solver.sat();
        assertThat(solver.model()).isNull();
        solver.reset();
        solver.add(f.parse("A & ~A"));
        solver.sat();
        assertThat(solver.model()).isNull();
        solver.reset();
        solver.add(f.parse("(A => (B & C)) & A & C & (C <=> ~B)"));
        solver.sat();
        assertThat(solver.model()).isNull();
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testEmptyModel(final SATSolver solver) {
        solver.reset();
        solver.add(f.verum());
        solver.sat();
        final Assignment model = solver.model();
        assertThat(model.literals()).isEmpty();
        assertThat(model.blockingClause(f)).isEqualTo(f.falsum());
        assertThat(solver.enumerateAllModels()).hasSize(1);
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testSimpleModel(final SATSolver solver) {
        solver.reset();
        solver.add(f.literal("A", true));
        solver.sat();
        Assignment model = solver.model();
        assertThat(model.literals()).containsExactly(f.literal("A", true));
        assertThat(solver.enumerateAllModels()).hasSize(1);
        solver.reset();
        solver.add(f.literal("A", false));
        solver.sat();
        model = solver.model();
        assertThat(model.literals()).containsExactly(f.literal("A", false));
        assertThat(solver.enumerateAllModels()).hasSize(1);
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testCNFFormula(final SATSolver solver) throws ParserException {
        solver.reset();
        final Formula formula = f.parse("(A|B|C) & (~A|~B|~C) & (A|~B|~C) & (~A|~B|C)");
        solver.add(formula);
        solver.sat();
        final Assignment model = solver.model();
        assertThat(formula.evaluate(model)).isTrue();
        assertThat(solver.enumerateAllModels()).hasSize(4);
        for (final Assignment assignment : solver.enumerateAllModels()) {
            assertThat(formula.evaluate(assignment)).isTrue();
        }
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testCNFWithAuxiliaryVars(final MiniSat solver) throws ParserException {
        solver.reset();
        final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
        final Formula cnf = formula.transform(new TseitinTransformation(0));
        solver.add(cnf);
        solver.sat();
        final Assignment model = solver.model();
        assertThat(formula.evaluate(model)).isTrue();
        final List<Assignment> allModels = solver.enumerateAllModels();
        assertThat(allModels).hasSize(4);
        if (solver.getConfig().isAuxiliaryVariablesInModels()) {
            assertThat(model.formula(f).variables()).isEqualTo(cnf.variables());
            for (final Assignment assignment : allModels) {
                assertThat(formula.evaluate(assignment)).isTrue();
                assertThat(assignment.formula(f).variables()).isEqualTo(cnf.variables());
            }
        } else {
            assertThat(model.formula(f).variables()).isEqualTo(formula.variables());
            for (final Assignment assignment : allModels) {
                assertThat(formula.evaluate(assignment)).isTrue();
                assertThat(assignment.formula(f).variables()).isEqualTo(formula.variables());
            }
        }
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testCNFWithAuxiliaryVarsRestrictedToOriginal(final SATSolver solver) throws ParserException {
        solver.reset();
        final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
        final Formula cnf = formula.transform(new TseitinTransformation(0));
        solver.add(cnf);
        solver.sat();
        final Assignment model = solver.model(formula.variables());
        assertThat(formula.evaluate(model)).isTrue();
        final List<Assignment> allModels = solver.enumerateAllModels(formula.variables());
        assertThat(allModels).hasSize(4);
        assertThat(model.formula(f).variables()).isEqualTo(formula.variables());
        for (final Assignment assignment : allModels) {
            assertThat(formula.evaluate(assignment)).isTrue();
            assertThat(assignment.formula(f).variables()).isEqualTo(formula.variables());
        }
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testNonCNFAllVars(final MiniSat solver) throws ParserException {
        solver.reset();
        final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
        solver.add(formula);
        solver.sat();
        final Assignment model = solver.model();
        assertThat(formula.evaluate(model)).isTrue();
        final List<Assignment> allModels = solver.enumerateAllModels();
        if (!solver.getConfig().isAuxiliaryVariablesInModels() || solver.getConfig().getCnfMethod() == MiniSatConfig.CNFMethod.FACTORY_CNF) {
            assertThat(allModels).hasSize(4);
            for (final Assignment assignment : allModels) {
                assertThat(formula.evaluate(assignment)).isTrue();
                assertThat(assignment.formula(f).variables()).isEqualTo(formula.variables());
            }
        } else {
            assertThat(allModels).hasSize(6);
            for (final Assignment assignment : allModels) {
                assertThat(formula.evaluate(assignment)).isTrue();
                assertThat(formula.variables()).isSubsetOf(assignment.formula(f).variables());
            }
        }
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testNonCNFOnlyFormulaVars(final SATSolver solver) throws ParserException {
        solver.reset();
        final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
        solver.add(formula);
        solver.sat();
        final Assignment model = solver.model(formula.variables());
        assertThat(formula.evaluate(model)).isTrue();
        assertThat(model.formula(f).variables()).isEqualTo(formula.variables());
        final List<Assignment> allModels = solver.enumerateAllModels(formula.variables());
        assertThat(allModels).hasSize(4);
        for (final Assignment assignment : allModels) {
            assertThat(formula.evaluate(assignment)).isTrue();
            assertThat(assignment.formula(f).variables()).isEqualTo(formula.variables());
        }
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testNonCNFRestrictedVars(final SATSolver solver) throws ParserException {
        solver.reset();
        final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
        final MiniSat miniSat = MiniSat.miniSat(f);
        miniSat.add(formula);
        solver.add(formula);
        solver.sat();
        final SortedSet<Variable> relevantVariables = new TreeSet<>(Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C")));
        final Assignment model = solver.model(relevantVariables);
        assertThat(miniSat.sat(model.literals())).isEqualTo(Tristate.TRUE);
        assertThat(model.formula(f).variables()).isEqualTo(relevantVariables);
        final List<Assignment> allModels = solver.enumerateAllModels(relevantVariables);
        assertThat(allModels).hasSize(2);
        for (final Assignment assignment : allModels) {
            assertThat(miniSat.sat(assignment.literals())).isEqualTo(Tristate.TRUE);
            assertThat(assignment.formula(f).variables()).isEqualTo(relevantVariables);
        }
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testNonCNFRestrictedAndAdditionalVars(final SATSolver solver) throws ParserException {
        solver.reset();
        final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
        final MiniSat miniSat = MiniSat.miniSat(f);
        miniSat.add(formula);
        solver.add(formula);
        solver.sat();
        final SortedSet<Variable> relevantVariables = new TreeSet<>(Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C")));
        final SortedSet<Variable> additionalVariables = new TreeSet<>(Arrays.asList(f.variable("D"), f.variable("X"), f.variable("Y")));
        final SortedSet<Variable> allVariables = new TreeSet<>(relevantVariables);
        allVariables.add(f.variable("D"));
        final Assignment model = solver.model(additionalVariables);
        assertThat(miniSat.sat(model.literals())).isEqualTo(Tristate.TRUE);
        assertThat(model.formula(f).variables()).containsExactly(f.variable("D"));
        final List<Assignment> allModels = solver.execute(ModelEnumerationFunction.builder().variables(relevantVariables).additionalVariables(additionalVariables).build());
        assertThat(allModels).hasSize(2);
        for (final Assignment assignment : allModels) {
            assertThat(miniSat.sat(assignment.literals())).isEqualTo(Tristate.TRUE);
            assertThat(assignment.formula(f).variables()).isEqualTo(allVariables);
        }
    }

    @ParameterizedTest
    @MethodSource("solvers")
    public void testUnsolvedFormula(final SATSolver solver) throws ParserException {
        solver.reset();
        final Formula formula = f.parse("(A => B & C) & (~A => C & ~D) & (C => (D & E | ~E & B)) & ~F");
        solver.add(formula);
        assertThatThrownBy(solver::model).isInstanceOf(IllegalStateException.class);
    }
}
