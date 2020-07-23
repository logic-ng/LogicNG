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

package org.logicng.solvers.sat;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.solvers.sat.MiniSatConfig.ClauseMinimization.BASIC;
import static org.logicng.solvers.sat.MiniSatConfig.ClauseMinimization.NONE;

import org.junit.jupiter.api.Test;
import org.logicng.LogicNGTest;
import org.logicng.LongRunningTag;
import org.logicng.TestWithExampleFormulas;
import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.CType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.ModelEnumerationHandler;
import org.logicng.handlers.NumberOfModelsHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.handlers.TimeoutModelEnumerationHandler;
import org.logicng.handlers.TimeoutSATHandler;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.functions.FormulaOnSolverFunction;
import org.logicng.solvers.functions.ModelEnumerationFunction;
import org.logicng.solvers.functions.UpZeroLiteralsFunction;
import org.logicng.testutils.PigeonHoleGenerator;
import org.logicng.util.FormulaHelper;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Unit tests for the SAT solvers.
 * @version 1.6
 * @since 1.0
 */
public class SATTest extends TestWithExampleFormulas implements LogicNGTest {

    private final FormulaFactory f;
    private final SATSolver[] solvers;
    private final PigeonHoleGenerator pg;
    private final PropositionalParser parser;
    private final String[] testStrings;

    public SATTest() {
        this.f = new FormulaFactory();
        this.pg = new PigeonHoleGenerator(this.f);
        this.parser = new PropositionalParser(this.f);
        this.solvers = new SATSolver[8];
        this.solvers[0] = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(true).build());
        this.solvers[1] = MiniSat.miniSat(this.f, MiniSatConfig.builder().incremental(false).build());
        this.solvers[2] = MiniSat.glucose(this.f, MiniSatConfig.builder().incremental(false).build(),
                GlucoseConfig.builder().build());
        this.solvers[3] = MiniSat.miniCard(this.f, MiniSatConfig.builder().incremental(true).build());
        this.solvers[4] = MiniSat.miniCard(this.f, MiniSatConfig.builder().incremental(false).build());
        this.solvers[5] = MiniSat.miniSat(this.f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        this.solvers[6] = MiniSat.miniSat(this.f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).auxiliaryVariablesInModels(false).build());
        this.solvers[7] = MiniSat.miniSat(this.f, MiniSatConfig.builder().cnfMethod(MiniSatConfig.CNFMethod.FULL_PG_ON_SOLVER).auxiliaryVariablesInModels(false).build());

        this.testStrings = new String[8];
        this.testStrings[0] = "MiniSat2Solver{result=UNDEF, incremental=true}";
        this.testStrings[1] = "MiniSat2Solver{result=UNDEF, incremental=false}";
        this.testStrings[2] = "GlucoseSyrup{result=UNDEF, incremental=false}";
        this.testStrings[3] = "MiniCard{result=UNDEF, incremental=true}";
        this.testStrings[4] = "MiniCard{result=UNDEF, incremental=false}";
        this.testStrings[5] = "MiniSat2Solver{result=UNDEF, incremental=true}";
        this.testStrings[6] = "MiniSat2Solver{result=UNDEF, incremental=true}";
        this.testStrings[7] = "MiniSat2Solver{result=UNDEF, incremental=true}";
    }

    @Test
    public void testTrue() {
        for (final SATSolver s : this.solvers) {
            s.add(this.TRUE);
            assertSolverSat(s);
            assertThat(s.model().size()).isEqualTo(0);
            s.reset();
        }
    }

    @Test
    public void testFalse() {
        for (final SATSolver s : this.solvers) {
            s.add(this.FALSE);
            assertSolverUnsat(s);
            assertThat(s.model()).isNull();
            s.reset();
        }
    }

    @Test
    public void testLiterals() {
        for (final SATSolver s : this.solvers) {
            s.add(this.A);
            assertSolverSat(s);
            assertThat(s.model().size()).isEqualTo(1);
            assertThat(s.model().evaluateLit(this.A)).isTrue();
            s.add(this.NA);
            assertSolverUnsat(s);
            s.reset();
            s.add(this.NA);
            assertSolverSat(s);
            assertThat(s.model().size()).isEqualTo(1);
            assertThat(s.model().evaluateLit(this.NA)).isTrue();
            s.reset();
        }
    }

    @Test
    public void testAnd1() {
        for (final SATSolver s : this.solvers) {
            s.add(this.AND1);
            assertSolverSat(s);
            assertThat(s.model().size()).isEqualTo(2);
            assertThat(s.model().evaluateLit(this.A)).isTrue();
            assertThat(s.model().evaluateLit(this.B)).isTrue();
            s.add(this.NOT1);
            assertSolverUnsat(s);
            assertThat(s.model()).isNull();
            s.reset();
        }
    }

    @Test
    public void testAnd2() {
        for (final SATSolver s : this.solvers) {
            final StandardProposition prop = new StandardProposition(this.f.and(this.f.literal("a", true), this.f.literal("b", false), this.f.literal("c", true), this.f.literal("d", false)));
            s.add(prop);
            assertSolverSat(s);
            assertThat(s.model().size()).isEqualTo(4);
            assertThat(s.model().evaluateLit(this.f.variable("a"))).isTrue();
            assertThat(s.model().evaluateLit(this.f.variable("b"))).isFalse();
            assertThat(s.model().evaluateLit(this.f.variable("c"))).isTrue();
            assertThat(s.model().evaluateLit(this.f.variable("d"))).isFalse();
            s.reset();
        }
    }

    @Test
    public void testAnd3() {
        for (final SATSolver s : this.solvers) {
            final List<Formula> formulas = new ArrayList<>(3);
            formulas.add(this.f.literal("a", true));
            formulas.add(this.f.literal("b", false));
            formulas.add(this.f.literal("a", false));
            formulas.add(this.f.literal("d", false));
            s.add(formulas);
            assertSolverUnsat(s);
            s.reset();
        }
    }

    @Test
    public void testFormula1() throws ParserException {
        for (final SATSolver s : this.solvers) {
            s.add(this.parser.parse("(x => y) & (~x => y) & (y => z) & (z => ~x)"));
            assertSolverSat(s);
            assertThat(s.model().size()).isEqualTo(3);
            assertThat(s.model().evaluateLit(this.f.variable("x"))).isFalse();
            assertThat(s.model().evaluateLit(this.f.variable("y"))).isTrue();
            assertThat(s.model().evaluateLit(this.f.variable("z"))).isTrue();
            s.add(this.f.variable("x"));
            assertSolverUnsat(s);
            s.reset();
        }
    }

    @Test
    public void testFormula2() throws ParserException {
        for (int i = 0; i < this.solvers.length - 1; i++) {
            final SATSolver s = this.solvers[i];
            s.add(this.parser.parse("(x => y) & (~x => y) & (y => z) & (z => ~x)"));
            final List<Assignment> models = s.enumerateAllModels();
            assertThat(models.size()).isEqualTo(1);
            assertThat(models.get(0).size()).isEqualTo(3);
            assertThat(models.get(0).evaluateLit(this.f.variable("x"))).isFalse();
            assertThat(models.get(0).evaluateLit(this.f.variable("y"))).isTrue();
            assertThat(models.get(0).evaluateLit(this.f.variable("z"))).isTrue();
            s.add(this.f.variable("x"));
            assertSolverUnsat(s);
            s.reset();
        }
    }

    @Test
    public void testFormula3() throws ParserException {
        for (int i = 0; i < this.solvers.length - 1; i++) {
            final SATSolver s = this.solvers[i];
            s.add(this.parser.parse("a | b"));
            final List<Assignment> models = s.execute(ModelEnumerationFunction.builder().additionalVariables(this.f.variable("c")).build());
            assertThat(models.size()).isEqualTo(3);
            assertThat(models.get(0).size()).isEqualTo(2);
            assertThat(models.get(1).size()).isEqualTo(2);
            assertThat(models.get(2).size()).isEqualTo(2);
            s.reset();
        }
    }

    @Test
    public void testCC1() {
        for (int i = 0; i < this.solvers.length - 1; i++) {
            final SATSolver s = this.solvers[i];
            final Variable[] lits = new Variable[100];
            for (int j = 0; j < lits.length; j++) {
                lits[j] = this.f.variable("x" + j);
            }
            s.add(this.f.exo(lits));
            final List<Assignment> models = s.enumerateAllModels(lits);
            assertThat(models.size()).isEqualTo(100);
            for (final Assignment m : models) {
                assertThat(m.positiveVariables().size()).isEqualTo(1);
            }
            s.reset();
        }
    }

    @Test
    public void testPBC() {
        for (final SATSolver s : this.solvers) {
            final List<Literal> lits = new ArrayList<>();
            final List<Integer> coeffs = new ArrayList<>();
            for (int i = 0; i < 5; i++) {
                lits.add(this.f.literal("x" + i, i % 2 == 0));
                coeffs.add(i + 1);
            }
            s.add(this.f.pbc(CType.GE, 10, lits, coeffs));
            assertSolverSat(s);
            s.reset();
        }
    }

    @Test
    public void testPartialModel() {
        for (final SATSolver s : this.solvers) {
            s.add(this.A);
            s.add(this.B);
            s.add(this.C);
            final Variable[] relevantVars = new Variable[2];
            relevantVars[0] = this.A;
            relevantVars[1] = this.B;
            assertSolverSat(s);
            final Assignment relModel = s.model(relevantVars);
            assertThat(relModel.negativeLiterals().isEmpty()).isTrue();
            assertThat(relModel.literals().contains(this.C)).isFalse();
            s.reset();
        }
    }

    @Test
    public void testModelEnumerationHandler() {
        for (final SATSolver s : this.solvers) {
            s.add(this.IMP3);
            try {
                final ModelEnumerationHandler handler = new ModelEnumerationHandler() {
                    private boolean aborted;

                    @Override
                    public boolean aborted() {
                        return this.aborted;
                    }

                    @Override
                    public void started() {
                        this.aborted = false;
                    }

                    @Override
                    public SATHandler satHandler() {
                        return null;
                    }

                    @Override
                    public boolean foundModel(final Assignment assignment) {
                        this.aborted = assignment.negativeLiterals().isEmpty();
                        return !this.aborted;
                    }

                    @Override
                    public boolean satSolverFinished() {
                        // nothing to do here
                        return true;
                    }
                };
                final List<Assignment> models = s.execute(ModelEnumerationFunction.builder().handler(handler).build());
                assertThat(models.isEmpty()).isFalse();
                assertThat(models.get(models.size() - 1).negativeLiterals().isEmpty()).isTrue();
                models.remove(models.size() - 1);
                for (final Assignment model : models) {
                    assertThat(model.negativeLiterals().isEmpty()).isFalse();
                }
            } catch (final Exception e) {
                assertThat(e instanceof UnsupportedOperationException).isTrue();
            }

            s.reset();
        }
    }

    @Test
    public void testWithRelaxation() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        final Formula one = parser.parse("a & b & (c | ~d)");
        final Formula two = parser.parse("~a | ~c");

        for (final SATSolver s : this.solvers) {
            s.add(one);
            s.addWithRelaxation(this.f.variable("d"), two);
            assertSolverSat(s);
            try {
                assertThat(s.enumerateAllModels().size()).isEqualTo(2);
            } catch (final Exception e) {
                assertThat(e instanceof UnsupportedOperationException).isTrue();
            }
            s.reset();

            s.add(one);
            s.addWithRelaxation(this.f.variable("d"), new StandardProposition(two));
            assertSolverSat(s);
            try {
                assertThat(s.enumerateAllModels().size()).isEqualTo(2);
            } catch (final Exception e) {
                assertThat(e instanceof UnsupportedOperationException).isTrue();
            }
            s.reset();

            s.add(one);
            s.addWithRelaxation(this.f.variable("d"), two);
            assertSolverSat(s);
            try {
                assertThat(s.enumerateAllModels().size()).isEqualTo(2);
            } catch (final Exception e) {
                assertThat(e instanceof UnsupportedOperationException).isTrue();
            }
            s.reset();

            s.add(one);
            s.addWithRelaxation(this.f.variable("d"), Arrays.asList(two, this.f.verum()));
            assertSolverSat(s);
            try {
                assertThat(s.enumerateAllModels().size()).isEqualTo(2);
            } catch (final Exception e) {
                assertThat(e instanceof UnsupportedOperationException).isTrue();
            }
            s.reset();
        }
    }

    @Test
    public void testRelexationFormulas() throws ParserException {
        for (final SATSolver s : this.solvers) {
            s.add(this.f.parse("a & (b | c)"));
            assertSolverSat(s);
            s.addWithRelaxation(this.f.variable("x"), this.f.parse("~a & ~b"));
            assertSolverSat(s);
            assertThat(s.model().positiveVariables()).contains(this.f.variable("x"));
            s.add(this.f.variable("x").negate());
            assertSolverUnsat(s);
        }
    }

    @Test
    public void testPigeonHole1() {
        for (final SATSolver s : this.solvers) {
            s.add(this.pg.generate(1));
            assertSolverUnsat(s);
            assertThat(s.model()).isNull();
            s.reset();
        }
    }

    @Test
    public void testPigeonHole2() {
        for (final SATSolver s : this.solvers) {
            s.add(this.pg.generate(2));
            assertSolverUnsat(s);
            assertSolverUnsat(s);
            assertThat(s.model()).isNull();
            s.reset();
        }
    }

    @Test
    public void testPigeonHole3() {
        for (final SATSolver s : this.solvers) {
            s.add(this.pg.generate(3));
            assertSolverUnsat(s);
            assertThat(s.model()).isNull();
            s.reset();
        }
    }

    @Test
    public void testPigeonHole4() {
        for (final SATSolver s : this.solvers) {
            s.add(this.pg.generate(4));
            assertSolverUnsat(s);
            assertThat(s.model()).isNull();
            s.reset();
        }
    }

    @Test
    public void testPigeonHole5() {
        for (final SATSolver s : this.solvers) {
            s.add(this.pg.generate(5));
            assertSolverUnsat(s);
            assertThat(s.model()).isNull();
            s.reset();
        }
    }

    @Test
    public void testPigeonHole6() {
        for (final SATSolver s : this.solvers) {
            s.add(this.pg.generate(6));
            assertSolverUnsat(s);
            assertThat(s.model()).isNull();
            s.reset();
        }
    }

    @Test
    public void testPigeonHole7() {
        for (final SATSolver s : this.solvers) {
            s.add(this.pg.generate(7));
            assertSolverUnsat(s);
            assertThat(s.model()).isNull();
            s.reset();
        }
    }

    @Test
    public void testDifferentClauseMinimizations() {
        final SATSolver[] moreSolvers = new SATSolver[6];
        moreSolvers[0] = MiniSat.miniSat(this.f, MiniSatConfig.builder().clMinimization(NONE).build());
        moreSolvers[1] = MiniSat.miniSat(this.f, MiniSatConfig.builder().clMinimization(BASIC).build());
        moreSolvers[2] = MiniSat.glucose(this.f, MiniSatConfig.builder().clMinimization(NONE).build(), GlucoseConfig.builder().build());
        moreSolvers[3] = MiniSat.glucose(this.f, MiniSatConfig.builder().clMinimization(BASIC).build(), GlucoseConfig.builder().build());
        moreSolvers[4] = MiniSat.miniCard(this.f, MiniSatConfig.builder().clMinimization(NONE).build());
        moreSolvers[5] = MiniSat.miniCard(this.f, MiniSatConfig.builder().clMinimization(BASIC).build());
        for (final SATSolver s : moreSolvers) {
            s.add(this.pg.generate(7));
            assertSolverUnsat(s);
            assertThat(s.model()).isNull();
        }
    }

    @Test
    public void testTimeoutSATHandlerSmall() {
        for (final SATSolver s : this.solvers) {
            s.add(this.IMP1);
            final TimeoutSATHandler handler = new TimeoutSATHandler(1000L);
            final Tristate result = s.sat(handler);
            assertThat(handler.aborted()).isFalse();
            assertThat(result).isEqualTo(Tristate.TRUE);
            s.reset();
        }
    }

    @Test
    public void testTimeoutSATHandlerLarge() {
        for (final SATSolver s : this.solvers) {
            s.add(this.pg.generate(10));
            final TimeoutSATHandler handler = new TimeoutSATHandler(1000L);
            final Tristate result = s.sat(handler);
            assertThat(handler.aborted()).isTrue();
            assertThat(result).isEqualTo(UNDEF);
            s.reset();
        }
    }

    @Test
    public void testDimacsFiles() throws IOException {
        final Map<String, Boolean> expectedResults = new HashMap<>();
        final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/sat/results.txt"));
        while (reader.ready()) {
            final String[] tokens = reader.readLine().split(";");
            expectedResults.put(tokens[0], Boolean.valueOf(tokens[1]));
        }
        final File testFolder = new File("src/test/resources/sat");
        final File[] files = testFolder.listFiles();
        assert files != null;
        for (final SATSolver solver : this.solvers) {
            for (final File file : files) {
                final String fileName = file.getName();
                if (fileName.endsWith(".cnf")) {
                    readCNF(solver, file);
                    final boolean res = solver.sat() == Tristate.TRUE;
                    assertThat(res).isEqualTo(expectedResults.get(fileName));
                }
            }
            solver.reset();
        }
    }

    private void readCNF(final SATSolver solver, final File file) throws IOException {
        solver.reset();
        final BufferedReader reader = new BufferedReader(new FileReader(file));
        while (reader.ready()) {
            final String line = reader.readLine();
            if (line.startsWith("p cnf")) {
                break;
            }
        }
        String[] tokens;
        final List<Literal> literals = new ArrayList<>();
        while (reader.ready()) {
            tokens = reader.readLine().split("\\s+");
            if (tokens.length >= 2) {
                assert "0".equals(tokens[tokens.length - 1]);
                literals.clear();
                for (int i = 0; i < tokens.length - 1; i++) {
                    if (!tokens[i].isEmpty()) {
                        final int parsedLit = Integer.parseInt(tokens[i]);
                        final String var = "v" + Math.abs(parsedLit);
                        literals.add(parsedLit > 0 ? this.f.literal(var, true) : this.f.literal(var, false));
                    }
                }
                if (!literals.isEmpty()) {
                    solver.add(this.f.or(literals));
                }
            }
        }
    }

    @Test
    public void testPigeonHoleWithReset() {
        for (final SATSolver s : this.solvers) {
            s.add(this.pg.generate(4));
            assertSolverUnsat(s);
            s.reset();
            s.add(this.pg.generate(5));
            assertSolverUnsat(s);
            s.reset();
            s.add(this.pg.generate(6));
            assertSolverUnsat(s);
            s.reset();
            s.add(this.pg.generate(7));
            assertSolverUnsat(s);
            s.reset();
        }
    }

    @Test
    public void testTimeoutModelEnumerationHandlerWithUNSATInstance() {
        for (final SATSolver solver : this.solvers) {
            solver.add(this.pg.generate(10));
            final TimeoutModelEnumerationHandler handler = new TimeoutModelEnumerationHandler(1000L);
            final List<Assignment> assignments = solver.execute(ModelEnumerationFunction.builder().handler(handler).build());
            assertThat(assignments).isEmpty();
            assertThat(handler.aborted()).isTrue();
            solver.reset();
        }
    }

    @Test
    public void testTimeoutModelEnumerationHandlerWithSATInstance() {
        for (final SATSolver solver : this.solvers) {
            final List<Variable> variables = new ArrayList<>();
            for (int i = 0; i < 1000; i++) {
                variables.add(this.f.variable("x" + i));
            }

            solver.add(this.f.exo(variables));
            TimeoutModelEnumerationHandler handler = new TimeoutModelEnumerationHandler(50L);
            solver.execute(ModelEnumerationFunction.builder().handler(handler).build());
            assertThat(handler.aborted()).isTrue();
            solver.reset();

            solver.add(this.f.exo(variables.subList(0, 5)));
            handler = new TimeoutModelEnumerationHandler(1000L);
            final List<Assignment> assignments = solver.execute(ModelEnumerationFunction.builder().handler(handler).build());
            assertThat(assignments).hasSize(5);
            assertThat(handler.aborted()).isFalse();
            solver.reset();
        }
    }

    @Test
    public void testModelEnumeration() {
        for (int i = 0; i < this.solvers.length - 1; i++) {
            final SATSolver s = this.solvers[i];
            final SortedSet<Variable> lits = new TreeSet<>();
            final SortedSet<Variable> firstFive = new TreeSet<>();
            for (int j = 0; j < 20; j++) {
                final Variable lit = this.f.variable("x" + j);
                lits.add(lit);
                if (j < 5) {
                    firstFive.add(lit);
                }
            }
            s.add(this.f.cc(CType.GE, 1, lits));

            final List<Assignment> models = s.execute(ModelEnumerationFunction.builder().variables(firstFive).additionalVariables(lits).build());
            assertThat(models.size()).isEqualTo(32);
            for (final Assignment model : models) {
                for (final Variable lit : lits) {
                    assertThat(model.positiveVariables().contains(lit) || model.negativeVariables().contains(lit)).isTrue();
                }
            }
            s.reset();
        }
    }

    @Test
    public void testModelEnumerationWithHandler01() {
        for (int i = 0; i < this.solvers.length - 1; i++) {
            final SATSolver s = this.solvers[i];
            final SortedSet<Variable> lits = new TreeSet<>();
            final SortedSet<Variable> firstFive = new TreeSet<>();
            for (int j = 0; j < 20; j++) {
                final Variable lit = this.f.variable("x" + j);
                lits.add(lit);
                if (j < 5) {
                    firstFive.add(lit);
                }
            }
            s.add(this.f.cc(CType.GE, 1, lits));

            final NumberOfModelsHandler handler = new NumberOfModelsHandler(29);
            final List<Assignment> modelsWithHandler = s.execute(ModelEnumerationFunction.builder().variables(firstFive).additionalVariables(lits).handler(handler).build());
            assertThat(handler.aborted()).isTrue();
            assertThat(modelsWithHandler.size()).isEqualTo(29);
            for (final Assignment model : modelsWithHandler) {
                for (final Variable lit : lits) {
                    assertThat(model.positiveVariables().contains(lit) || model.negativeVariables().contains(lit)).isTrue();
                }
            }
            s.reset();
        }
    }

    @Test
    public void testModelEnumerationWithHandler02() {
        for (int i = 0; i < this.solvers.length - 1; i++) {
            final SATSolver s = this.solvers[i];
            final SortedSet<Variable> lits = new TreeSet<>();
            final SortedSet<Variable> firstFive = new TreeSet<>();
            for (int j = 0; j < 20; j++) {
                final Variable lit = this.f.variable("x" + j);
                lits.add(lit);
                if (j < 5) {
                    firstFive.add(lit);
                }
            }
            s.add(this.f.cc(CType.GE, 1, lits));

            final NumberOfModelsHandler handler = new NumberOfModelsHandler(29);
            final List<Assignment> modelsWithHandler = s.execute(ModelEnumerationFunction.builder().additionalVariables(Collections.singletonList(firstFive.first())).handler(handler).build());
            assertThat(handler.aborted()).isTrue();
            assertThat(modelsWithHandler.size()).isEqualTo(29);
            for (final Assignment model : modelsWithHandler) {
                for (final Variable lit : lits) {
                    assertThat(model.positiveVariables().contains(lit) || model.negativeVariables().contains(lit)).isTrue();
                }
            }
            s.reset();
        }
    }

    @Test
    public void testEmptyEnumeration() {
        for (int i = 0; i < this.solvers.length - 1; i++) {
            final SATSolver s = this.solvers[i];
            s.add(this.f.falsum());
            final List<Assignment> models = s.enumerateAllModels();
            assertThat(models.isEmpty()).isTrue();

            s.reset();
        }
    }

    @Test
    public void testNumberOfModelHandler() {
        for (int i = 0; i < this.solvers.length - 1; i++) {
            final SATSolver s = this.solvers[i];
            final Variable[] lits = new Variable[100];
            for (int j = 0; j < lits.length; j++) {
                lits[j] = this.f.variable("x" + j);
            }
            s.add(this.f.exo(lits));
            NumberOfModelsHandler handler = new NumberOfModelsHandler(100);
            List<Assignment> models = s.execute(ModelEnumerationFunction.builder().variables(lits).handler(handler).build());
            assertThat(handler.aborted()).isTrue();
            assertThat(models.size()).isEqualTo(100);
            for (final Assignment m : models) {
                assertThat(m.positiveVariables().size()).isEqualTo(1);
            }
            s.reset();

            s.add(this.f.exo(lits));
            handler = new NumberOfModelsHandler(200);
            models = s.execute(ModelEnumerationFunction.builder().variables(lits).handler(handler).build());
            assertThat(handler.aborted()).isFalse();
            assertThat(models.size()).isEqualTo(100);
            for (final Assignment m : models) {
                assertThat(m.positiveVariables().size()).isEqualTo(1);
            }
            s.reset();

            s.add(this.f.exo(lits));
            handler = new NumberOfModelsHandler(50);
            models = s.execute(ModelEnumerationFunction.builder().variables(lits).handler(handler).build());
            assertThat(handler.aborted()).isTrue();
            assertThat(models.size()).isEqualTo(50);
            for (final Assignment m : models) {
                assertThat(m.positiveVariables().size()).isEqualTo(1);
            }
            s.reset();

            s.add(this.f.exo(lits));
            handler = new NumberOfModelsHandler(1);
            models = s.execute(ModelEnumerationFunction.builder().variables(lits).handler(handler).build());
            assertThat(handler.aborted()).isTrue();
            assertThat(models.size()).isEqualTo(1);
            for (final Assignment m : models) {
                assertThat(m.positiveVariables().size()).isEqualTo(1);
            }
            s.reset();
        }
    }

    @Test
    public void testIllegalHandler() {
        assertThatThrownBy(() -> new NumberOfModelsHandler(0)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testModelBeforeSolving() {
        final MiniSat solver = MiniSat.miniSat(this.f);
        assertThatThrownBy(solver::model).isInstanceOf(IllegalStateException.class);
    }

    @Test
    public void testToString() {
        for (int i = 0; i < this.solvers.length; i++) {
            assertThat(this.solvers[i].toString()).isEqualTo(this.testStrings[i]);
        }
    }

    @Test
    public void testKnownVariables() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        final Formula phi = parser.parse("x1 & x2 & x3 & (x4 | ~x5)");
        final SATSolver minisat = MiniSat.miniSat(this.f);
        final SATSolver minicard = MiniSat.miniCard(this.f);
        minisat.add(phi);
        minicard.add(phi);
        final SortedSet<Variable> expected = new TreeSet<>(Arrays.asList(
                this.f.variable("x1"),
                this.f.variable("x2"),
                this.f.variable("x3"),
                this.f.variable("x4"),
                this.f.variable("x5")));
        assertThat(minisat.knownVariables()).isEqualTo(expected);
        assertThat(minicard.knownVariables()).isEqualTo(expected);

        final SolverState state = minisat.saveState();
        final SolverState stateCard = minicard.saveState();
        minisat.add(this.f.variable("x6"));
        minicard.add(this.f.variable("x6"));
        final SortedSet<Variable> expected2 = new TreeSet<>(Arrays.asList(
                this.f.variable("x1"),
                this.f.variable("x2"),
                this.f.variable("x3"),
                this.f.variable("x4"),
                this.f.variable("x5"),
                this.f.variable("x6")));
        assertThat(minisat.knownVariables()).isEqualTo(expected2);
        assertThat(minicard.knownVariables()).isEqualTo(expected2);

        // load state for minisat
        minisat.loadState(state);
        minicard.loadState(stateCard);
        assertThat(minisat.knownVariables()).isEqualTo(expected);
        assertThat(minicard.knownVariables()).isEqualTo(expected);
    }

    @Test
    public void testAddWithoutUnknown() throws ParserException {
        final PropositionalParser parser = new PropositionalParser(this.f);
        final Formula phi = parser.parse("x1 & (~x2 | x3) & (x4 | ~x5)");
        final SortedSet<Variable> phiVars = new TreeSet<>(Arrays.asList(
                this.f.variable("x1"),
                this.f.variable("x2"),
                this.f.variable("x3"),
                this.f.variable("x4"),
                this.f.variable("x5")));
        final Formula add1 = parser.parse("x1 | x6 | x7");
        final Formula add2 = parser.parse("~x1 | ~x6 | x8");
        final Formula add3 = parser.parse("x2 & ~x3 | x7");
        final Formula add4 = parser.parse("x8 | x9");
        final SATSolver minisat = MiniSat.miniSat(this.f);
        final SATSolver minicard = MiniSat.miniCard(this.f);
        final SATSolver[] solvers = new SATSolver[]{minisat, minicard};
        for (final SATSolver solver : solvers) {
            solver.add(phi);
            solver.addWithoutUnknown(add1);
            assertThat(solver.sat()).isEqualTo(Tristate.TRUE);
            assertThat(solver.model().formula(this.f).variables()).isEqualTo(phiVars);
            solver.addWithoutUnknown(add2);
            assertThat(solver.sat()).isEqualTo(Tristate.TRUE);
            assertThat(solver.model().formula(this.f).variables()).isEqualTo(phiVars);
            if (solver instanceof MiniSat) {
                final SolverState state = solver.saveState();
                solver.addWithoutUnknown(add3);
                assertThat(solver.sat()).isEqualTo(Tristate.FALSE);
                solver.loadState(state);
                solver.add(add1);
                assertThat(solver.sat()).isEqualTo(Tristate.TRUE);
                assertThat(solver.model().formula(this.f).variables().containsAll(Arrays.asList(this.f.variable("x6"), this.f.variable("x7")))).isTrue();
                solver.loadState(state);
                solver.sat();
                assertThat(solver.model().formula(this.f).variables()).isEqualTo(phiVars);
            } else {
                solver.add(add1);
                assertThat(solver.sat()).isEqualTo(Tristate.TRUE);
                assertThat(solver.model().formula(this.f).variables().containsAll(Arrays.asList(this.f.variable("x6"), this.f.variable("x7")))).isTrue();
                solver.add(this.f.variable("x7"));
                assertThat(solver.sat()).isEqualTo(Tristate.TRUE);
                assertThat(solver.model().formula(this.f).variables().containsAll(Arrays.asList(this.f.variable("x6"), this.f.variable("x7")))).isTrue();
                solver.addWithoutUnknown(add4);
                assertThat(solver.sat()).isEqualTo(Tristate.FALSE);
            }
        }
    }

    @Test
    public void testUPZeroLiteralsForUndefState() {
        assertThatThrownBy(() -> {
            final SATSolver solver = MiniSat.miniSat(this.f);
            solver.add(this.f.parse("a & b"));
            solver.execute(UpZeroLiteralsFunction.get());
        }).isInstanceOf(IllegalStateException.class)
                .hasMessage("Cannot get unit propagated literals on level 0 as long as the formula is not solved.  Call 'sat' first.");
    }

    @Test
    public void testUPZeroLiteralsUNSAT() throws ParserException {
        final Formula formula = this.parser.parse("a & (a => b) & (b => c) & (c => ~a)");
        for (final SATSolver solver : this.solvers) {
            solver.reset();
            solver.add(formula);
            solver.sat();
            final SortedSet<Literal> upLiterals = solver.execute(UpZeroLiteralsFunction.get());
            assertThat(upLiterals).isNull();
        }
    }

    @Test
    public void testUPZeroLiterals() throws ParserException {
        // Note: The complete unit propagated set of literals on level 0 depends on each solver's added learned clauses during the solving process
        final Map<Formula, SortedSet<Literal>> expectedSubsets = new HashMap<>();
        expectedSubsets.put(this.f.verum(), new TreeSet<>());
        expectedSubsets.put(this.parser.parse("a"), new TreeSet<>(Collections.singletonList(this.f.literal("a", true))));
        expectedSubsets.put(this.parser.parse("a | b"), new TreeSet<>());
        expectedSubsets.put(this.parser.parse("a & b"), new TreeSet<>(Arrays.asList(this.f.literal("a", true), this.f.literal("b", true))));
        expectedSubsets.put(this.parser.parse("a & ~b"), new TreeSet<>(Arrays.asList(this.f.literal("a", true), this.f.literal("b", false))));
        expectedSubsets.put(this.parser.parse("(a | c) & ~b"), new TreeSet<>(Collections.singletonList(this.f.literal("b", false))));
        expectedSubsets.put(this.parser.parse("(b | c) & ~b & (~c | d)"), new TreeSet<>(Arrays.asList(
                this.f.literal("b", false), this.f.literal("c", true), this.f.literal("d", true))));
        for (final SATSolver solver : this.solvers) {
            for (final Formula formula : expectedSubsets.keySet()) {
                solver.reset();
                solver.add(formula);
                final boolean res = solver.sat() == Tristate.TRUE;
                assertThat(res).isTrue();
                final SortedSet<Literal> upLiterals = solver.execute(UpZeroLiteralsFunction.get());
                assertThat(upLiterals).containsAll(expectedSubsets.get(formula));
            }
        }
    }

    @Test
    public void testUPZeroLiteralsDimacsFiles() throws IOException {
        final File testFolder = new File("src/test/resources/sat");
        final File[] files = testFolder.listFiles();
        assert files != null;
        for (final SATSolver solver : this.solvers) {
            for (final File file : files) {
                final String fileName = file.getName();
                if (fileName.endsWith(".cnf")) {
                    readCNF(solver, file);
                    final boolean res = solver.sat() == Tristate.TRUE;
                    if (res) {
                        final SortedSet<Literal> upZeroLiterals = solver.execute(UpZeroLiteralsFunction.get());
                        final List<Literal> negations = new ArrayList<>(upZeroLiterals.size());
                        for (final Literal lit : upZeroLiterals) {
                            negations.add(lit.negate());
                        }
                        solver.add(this.f.or(negations));
                        // Test if CNF implies identified unit propagated literals on level zero, i.e., each literal is a backbone literal
                        assertThat(solver.sat()).isEqualTo(Tristate.FALSE);
                    }
                }
            }
            solver.reset();
        }
    }

    @Test
    public void testFormulaOnSolver() throws ParserException {
        for (final SATSolver solver : this.solvers) {
            if (solver instanceof MiniSat) {
                final PseudoBooleanParser p = new PseudoBooleanParser(this.f);
                final Set<Formula> formulas = new LinkedHashSet<>();
                formulas.add(p.parse("A | B | C"));
                formulas.add(p.parse("~A | ~B | ~C"));
                formulas.add(p.parse("A | ~B"));
                formulas.add(p.parse("A"));
                solver.add(formulas);
                compareFormulas(formulas, solver.execute(FormulaOnSolverFunction.get()));
                formulas.add(p.parse("~A | C"));
                solver.reset();
                solver.add(formulas);
                compareFormulas(formulas, solver.execute(FormulaOnSolverFunction.get()));
                final Formula formula = p.parse("C + D + E <= 2");
                formulas.add(formula);
                solver.add(formula);
                compareFormulas(formulas, solver.execute(FormulaOnSolverFunction.get()));
            }
        }
    }

    @Test
    public void testFormulaOnSolverWithContradiction() throws ParserException {
        for (final SATSolver solver : this.solvers) {
            if (solver instanceof MiniSat) {
                solver.add(this.f.variable("A"));
                solver.add(this.f.variable("B"));
                solver.add(this.f.parse("C & (~A | ~B)"));
                assertThat(solver.execute(FormulaOnSolverFunction.get()))
                        .containsExactlyInAnyOrder(this.f.variable("A"), this.f.variable("B"), this.f.variable("C"), this.f.falsum());
                solver.reset();
                solver.add(this.f.parse("A <=> B"));
                solver.add(this.f.parse("B <=> ~A"));
                assertThat(solver.execute(FormulaOnSolverFunction.get()))
                        .containsExactlyInAnyOrder(this.f.parse("A | ~B"), this.f.parse("~A | B"), this.f.parse("~B | ~A"), this.f.parse("B | A"));
                solver.sat();
                assertThat(solver.execute(FormulaOnSolverFunction.get()))
                        .containsExactlyInAnyOrder(this.f.parse("A | ~B"), this.f.parse("~A | B"), this.f.parse("~B | ~A"), this.f.parse("B | A"),
                                this.f.variable("A"), this.f.variable("B"), this.f.falsum());
            }
        }
    }

    @Test
    public void testSelectionOrderSimple01() throws ParserException {
        for (final SATSolver solver : this.solvers) {
            final Formula formula = this.parser.parse("~(x <=> y)");
            solver.add(formula);

            List<Literal> selectionOrder = Arrays.asList(this.X, this.Y);
            assertThat(solver.satWithSelectionOrder(selectionOrder)).isEqualTo(Tristate.TRUE);
            Assignment assignment = solver.model();
            assertThat(assignment.literals()).containsExactlyInAnyOrder(this.X, this.NY);
            testLocalMinimum(solver, assignment, selectionOrder);
            testHighestLexicographicalAssignment(solver, assignment, selectionOrder);

            solver.setSolverToUndef();
            selectionOrder = Arrays.asList(this.Y, this.X);
            assertThat(solver.satWithSelectionOrder(selectionOrder)).isEqualTo(Tristate.TRUE);
            assignment = solver.model();
            assertThat(assignment.literals()).containsExactlyInAnyOrder(this.Y, this.NX);
            testLocalMinimum(solver, assignment, selectionOrder);
            testHighestLexicographicalAssignment(solver, assignment, selectionOrder);

            solver.setSolverToUndef();
            selectionOrder = Collections.singletonList(this.NX);
            assertThat(solver.sat(selectionOrder)).isEqualTo(Tristate.TRUE);
            assignment = solver.model();
            assertThat(assignment.literals()).containsExactlyInAnyOrder(this.Y, this.NX);
            testLocalMinimum(solver, assignment, selectionOrder);
            testHighestLexicographicalAssignment(solver, assignment, selectionOrder);

            solver.setSolverToUndef();
            selectionOrder = Arrays.asList(this.NY, this.NX);
            assertThat(solver.satWithSelectionOrder(selectionOrder)).isEqualTo(Tristate.TRUE);
            assignment = solver.model();
            assertThat(assignment.literals()).containsExactlyInAnyOrder(this.X, this.NY);
            testLocalMinimum(solver, assignment, selectionOrder);
            testHighestLexicographicalAssignment(solver, assignment, selectionOrder);

            solver.reset();
        }
    }

    @Test
    public void testSelectionOrderSimple02() {
        for (final SATSolver solver : this.solvers) {
            final List<Variable> literals = new ArrayList<>();
            for (int i = 0; i < 5; i++) {
                final Variable lit = this.f.variable("x" + i);
                literals.add(lit);
            }
            solver.add(this.f.cc(CType.EQ, 2, literals));

            for (int i = 0; i < 10; ++i) {
                assertThat(solver.satWithSelectionOrder(literals)).isEqualTo(Tristate.TRUE);
                final Assignment assignment = solver.model();
                testLocalMinimum(solver, assignment, literals);
                testHighestLexicographicalAssignment(solver, assignment, literals);
                solver.add(assignment.blockingClause(this.f, literals));
            }

            solver.reset();
            solver.add(this.f.cc(CType.EQ, 2, literals));
            final List<Literal> selectionOrder02 = Arrays.asList(
                    this.f.literal("x4", true), this.f.literal("x0", false),
                    this.f.literal("x1", true), this.f.literal("x2", true),
                    this.f.literal("x3", true));

            for (int i = 0; i < 10; ++i) {
                assertThat(solver.satWithSelectionOrder(selectionOrder02)).isEqualTo(Tristate.TRUE);
                final Assignment assignment = solver.model();
                testLocalMinimum(solver, assignment, selectionOrder02);
                testHighestLexicographicalAssignment(solver, assignment, selectionOrder02);
                solver.add(assignment.blockingClause(this.f, selectionOrder02));
            }

            solver.reset();
        }
    }

    @Test
    @LongRunningTag
    public void testDimacsFilesWithSelectionOrder() throws IOException {
        final Map<String, Boolean> expectedResults = new HashMap<>();
        final BufferedReader reader = new BufferedReader(new FileReader("src/test/resources/sat/results.txt"));
        while (reader.ready()) {
            final String[] tokens = reader.readLine().split(";");
            expectedResults.put(tokens[0], Boolean.valueOf(tokens[1]));
        }
        final File testFolder = new File("src/test/resources/sat");
        final File[] files = testFolder.listFiles();
        assert files != null;
        for (final SATSolver solver : this.solvers) {
            for (final File file : files) {
                final String fileName = file.getName();
                if (fileName.endsWith(".cnf")) {
                    readCNF(solver, file);
                    final List<Literal> selectionOrder = new ArrayList<>();
                    for (final Variable var : FormulaHelper.variables(solver.execute(FormulaOnSolverFunction.get()))) {
                        if (selectionOrder.size() < 10) {
                            selectionOrder.add(var.negate());
                        }
                    }
                    final boolean res = solver.satWithSelectionOrder(selectionOrder) == Tristate.TRUE;
                    assertThat(res).isEqualTo(expectedResults.get(fileName));
                    if (expectedResults.get(fileName)) {
                        final Assignment assignment = solver.model();
                        testLocalMinimum(solver, assignment, selectionOrder);
                        testHighestLexicographicalAssignment(solver, assignment, selectionOrder);
                    }
                }
            }
            solver.reset();
        }
    }

    @Test
    public void testModelEnumerationWithAdditionalVariables() throws ParserException {
        final SATSolver solver = MiniSat.miniSat(this.f);
        solver.add(this.f.parse("A | B | C | D | E"));
        final List<Assignment> models = solver.execute(ModelEnumerationFunction.builder()
                .variables(Arrays.asList(this.f.variable("A"), this.f.variable("B")))
                .additionalVariables(Arrays.asList(this.f.variable("B"), this.f.variable("C"))).build());
        for (final Assignment model : models) {
            int countB = 0;
            for (final Variable variable : model.positiveVariables()) {
                if (variable.name().equals("B")) {
                    countB++;
                }
            }
            assertThat(countB).isLessThan(2);
            countB = 0;
            for (final Variable variable : model.negativeVariables()) {
                if (variable.name().equals("B")) {
                    countB++;
                }
            }
            assertThat(countB).isLessThan(2);
        }

    }

    private void compareFormulas(final Collection<Formula> original, final Collection<Formula> solver) {
        final SortedSet<Variable> vars = new TreeSet<>();
        for (final Formula formula : original) {
            vars.addAll(formula.variables());
        }
        final MiniSat miniSat = MiniSat.miniSat(this.f);
        miniSat.add(original);
        final List<Assignment> models1 = miniSat.enumerateAllModels(vars);
        miniSat.reset();
        miniSat.add(solver);
        final List<Assignment> models2 = miniSat.enumerateAllModels(vars);
        assertThat(models1).hasSameElementsAs(models2);
    }

    /**
     * Tests if the given satisfying assignment is a local minimal model regarding the given relevant literals, i.e.
     * there is no variable in the assignment, contained in the relevant literals, that can be flipped without
     * resulting in an unsatisfying assignment.
     * @param solver           the solver with the loaded formulas
     * @param assignment       the satisfying assignment
     * @param relevantLiterals the relevant literals.
     */
    private void testLocalMinimum(final SATSolver solver, final Assignment assignment, final Collection<? extends Literal> relevantLiterals) {
        final SortedSet<Literal> literals = assignment.literals();
        for (final Literal lit : relevantLiterals) {
            if (!literals.contains(lit)) {
                final SortedSet<Literal> literalsWithFlip = new TreeSet<>(literals);
                literalsWithFlip.remove(lit.negate());
                literalsWithFlip.add(lit);
                assertThat(solver.sat(literalsWithFlip)).isEqualTo(Tristate.FALSE);
            }
        }
    }

    /**
     * Tests if the given satisfying assignment is the highest assignment in the lexicographical order based on the given
     * literals order.
     * @param solver     the solver with the loaded formulas
     * @param assignment the satisfying assignment
     * @param order      the literals order
     */
    private void testHighestLexicographicalAssignment(final SATSolver solver, final Assignment assignment, final List<? extends Literal> order) {
        final SortedSet<Literal> literals = assignment.literals();
        final List<Literal> orderSublist = new ArrayList<>();
        for (final Literal lit : order) {
            final boolean containsLit = literals.contains(lit);
            if (!containsLit) {
                final SortedSet<Literal> orderSubsetWithFlip = new TreeSet<>(orderSublist);
                orderSubsetWithFlip.remove(lit.negate());
                orderSubsetWithFlip.add(lit);
                assertThat(solver.sat(orderSubsetWithFlip)).isEqualTo(Tristate.FALSE);
            }
            orderSublist.add(containsLit ? lit : lit.negate());
        }
    }
}
