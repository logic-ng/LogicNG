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
//  Copyright 2015 Christoph Zengler                                     //
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

package org.logicng.explanations.drup;

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;

import org.assertj.core.api.SoftAssertions;
import org.junit.jupiter.api.Test;
import org.logicng.LogicNGTest;
import org.logicng.LongRunningTag;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.UNSATCore;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.DimacsReader;
import org.logicng.propositions.ExtendedProposition;
import org.logicng.propositions.Proposition;
import org.logicng.propositions.PropositionBackpack;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;
import org.logicng.solvers.SolverState;
import org.logicng.solvers.sat.GlucoseConfig;
import org.logicng.solvers.sat.MiniSatConfig;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Unit tests for {@link DRUPTrim}.
 * @version 2.0.0
 * @since 1.3
 */
public class DRUPTest implements LogicNGTest {

    private final FormulaFactory f = new FormulaFactory();

    private final SATSolver[] solvers;

    public DRUPTest() {
        this.solvers = new SATSolver[3];
        this.solvers[0] = MiniSat.miniSat(this.f, MiniSatConfig.builder().proofGeneration(true).incremental(true).build());
        this.solvers[1] = MiniSat.miniSat(this.f, MiniSatConfig.builder().proofGeneration(true).incremental(false).build());
        this.solvers[2] = MiniSat.glucose(this.f, MiniSatConfig.builder().proofGeneration(true).incremental(false).build(),
                GlucoseConfig.builder().build());
    }

    @Test
    public void testUnsatCoresFromDimacs() throws IOException {
        final List<List<Formula>> cnfs = new ArrayList<>(3);
        cnfs.add(DimacsReader.readCNF("src/test/resources/drup/simple_input.cnf", this.f));
        cnfs.add(DimacsReader.readCNF("src/test/resources/drup/pg4_input.cnf", this.f));
        cnfs.add(DimacsReader.readCNF("src/test/resources/drup/avg_input.cnf", this.f, "var"));

        for (final SATSolver solver : this.solvers) {
            for (final List<Formula> cnf : cnfs) {
                solver.add(cnf);
                assertSolverUnsat(solver);
                final UNSATCore<Proposition> unsatCore = solver.unsatCore();
                verifyCore(unsatCore, cnf);
                solver.reset();
            }
        }
    }

    @Test
    @LongRunningTag
    public void testUnsatCoresFromLargeTestset() throws IOException {
        final File testFolder = new File("src/test/resources/sat");
        final File[] files = testFolder.listFiles();
        assert files != null;
        int count = 0;
        for (final SATSolver solver : this.solvers) {
            for (final File file : files) {
                final String fileName = file.getName();
                if (fileName.endsWith(".cnf")) {
                    final List<Formula> cnf = DimacsReader.readCNF(file, this.f);
                    solver.add(cnf);
                    if (solver.sat() == FALSE) {
                        final UNSATCore<Proposition> unsatCore = solver.unsatCore();
                        verifyCore(unsatCore, cnf);
                        count++;
                    }
                    solver.reset();
                }
            }
            solver.reset();
        }
        assertThat(count).isEqualTo(11 * this.solvers.length);
    }

    @Test
    public void testUnsatCoresAimTestset() throws IOException {
        final File testFolder = new File("src/test/resources/sat/unsat");
        final File[] files = testFolder.listFiles();
        assert files != null;
        int count = 0;
        for (final SATSolver solver : this.solvers) {
            for (final File file : files) {
                final String fileName = file.getName();
                if (fileName.endsWith(".cnf")) {
                    final List<Formula> cnf = DimacsReader.readCNF(file, this.f);
                    solver.add(cnf);
                    assertSolverUnsat(solver);
                    final UNSATCore<Proposition> unsatCore = solver.unsatCore();
                    verifyCore(unsatCore, cnf);
                    solver.reset();
                    count++;
                }
            }
            solver.reset();
        }
        assertThat(count).isEqualTo(36 * this.solvers.length);
    }

    @Test
    public void testPropositionHandling() throws ParserException {
        final List<Proposition> propositions = new ArrayList<>();
        propositions.add(new StandardProposition("P1", this.f.parse("((a & b) => c) &  ((a & b) => d)")));
        propositions.add(new StandardProposition("P2", this.f.parse("(c & d) <=> ~e")));
        propositions.add(new StandardProposition("P3", this.f.parse("~e => f | g")));
        propositions.add(new StandardProposition("P4", this.f.parse("(f => ~a) & (g => ~b) & p & q")));
        propositions.add(new StandardProposition("P5", this.f.parse("a => b")));
        propositions.add(new StandardProposition("P6", this.f.parse("a")));
        propositions.add(new StandardProposition("P7", this.f.parse("g | h")));
        propositions.add(new StandardProposition("P8", this.f.parse("(x => ~y | z) & (z | w)")));

        for (final SATSolver solver : this.solvers) {
            solver.addPropositions(propositions);
            assertThat(solver.sat()).isEqualTo(FALSE);
            final UNSATCore<Proposition> unsatCore = solver.unsatCore();
            assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(propositions.get(0), propositions.get(1),
                    propositions.get(2), propositions.get(3), propositions.get(4), propositions.get(5));
            solver.reset();
        }
    }

    @Test
    public void testPropositionIncDec() throws ParserException {
        final SATSolver solver = this.solvers[0];
        final StandardProposition p1 = new StandardProposition("P1", this.f.parse("((a & b) => c) &  ((a & b) => d)"));
        final StandardProposition p2 = new StandardProposition("P2", this.f.parse("(c & d) <=> ~e"));
        final StandardProposition p3 = new StandardProposition("P3", this.f.parse("~e => f | g"));
        final StandardProposition p4 = new StandardProposition("P4", this.f.parse("(f => ~a) & (g => ~b) & p & q"));
        final StandardProposition p5 = new StandardProposition("P5", this.f.parse("a => b"));
        final StandardProposition p6 = new StandardProposition("P6", this.f.parse("a"));
        final StandardProposition p7 = new StandardProposition("P7", this.f.parse("g | h"));
        final StandardProposition p8 = new StandardProposition("P8", this.f.parse("(x => ~y | z) & (z | w)"));
        final StandardProposition p9 = new StandardProposition("P9", this.f.parse("a & b"));
        final StandardProposition p10 = new StandardProposition("P10", this.f.parse("(p => q) & p"));
        final StandardProposition p11 = new StandardProposition("P11", this.f.parse("a & ~q"));

        solver.addPropositions(p1, p2, p3, p4);
        final SolverState state1 = solver.saveState();
        solver.addPropositions(p5, p6);
        final SolverState state2 = solver.saveState();
        solver.addPropositions(p7, p8);

        assertThat(solver.sat()).isEqualTo(FALSE);
        UNSATCore<Proposition> unsatCore = solver.unsatCore();
        assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(p1, p2, p3, p4, p5, p6);

        solver.loadState(state2);
        assertThat(solver.sat()).isEqualTo(FALSE);
        unsatCore = solver.unsatCore();
        assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(p1, p2, p3, p4, p5, p6);

        solver.loadState(state1);
        solver.add(p9);
        assertThat(solver.sat()).isEqualTo(FALSE);
        unsatCore = solver.unsatCore();
        assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(p1, p2, p3, p4, p9);

        solver.loadState(state1);
        solver.add(p5);
        solver.add(p6);
        assertThat(solver.sat()).isEqualTo(FALSE);
        unsatCore = solver.unsatCore();
        assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(p1, p2, p3, p4, p5, p6);

        solver.loadState(state1);
        solver.add(p10);
        solver.add(p11);
        assertThat(solver.sat()).isEqualTo(FALSE);
        unsatCore = solver.unsatCore();
        assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(p4, p11);
    }

    @Test
    public void testTrivialCasesPropositions() throws ParserException {
        for (final SATSolver solver : this.solvers) {
            assertSolverSat(solver);
            final StandardProposition p1 = new StandardProposition("P1", this.f.parse("$false"));
            solver.add(p1);
            assertSolverUnsat(solver);
            UNSATCore<Proposition> unsatCore = solver.unsatCore();
            assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(p1);

            solver.reset();
            assertSolverSat(solver);
            final StandardProposition p2 = new StandardProposition("P2", this.f.parse("a"));
            solver.add(p2);
            assertSolverSat(solver);
            final StandardProposition p3 = new StandardProposition("P3", this.f.parse("~a"));
            solver.add(p3);
            assertSolverUnsat(solver);
            unsatCore = solver.unsatCore();
            assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(p2, p3);
        }
    }

    @Test
    public void testCoreAndAssumptions() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final SATSolver solver = MiniSat.miniSat(f, MiniSatConfig.builder().proofGeneration(true).cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        final StandardProposition p1 = new StandardProposition(f.parse("A => B"));
        final StandardProposition p2 = new StandardProposition(f.parse("A & B => G"));
        final StandardProposition p3 = new StandardProposition(f.or(f.literal("X", false), f.literal("A", true)));
        final StandardProposition p4 = new StandardProposition(f.or(f.literal("X", false), f.literal("G", false)));
        final StandardProposition p5 = new StandardProposition(f.literal("G", false));
        final StandardProposition p6 = new StandardProposition(f.literal("A", true));
        solver.add(p1);
        solver.add(p2);
        solver.add(p3);
        solver.add(p4);

        // Assumption call
        solver.sat(f.variable("X"));

        solver.add(p5);
        solver.add(p6);
        solver.sat();
        final UNSATCore<Proposition> unsatCore = solver.unsatCore();
        assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(p1, p2, p5, p6);
    }

    @Test
    public void testCoreAndAssumptions2() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final MiniSat solver = MiniSat.miniSat(f, MiniSatConfig.builder().proofGeneration(true).cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());

        solver.add(f.parse("~C => D"));
        solver.add(f.parse("C => D"));
        solver.add(f.parse("D => B | A"));
        solver.add(f.parse("B => X"));
        solver.add(f.parse("B => ~X"));
        solver.sat(f.literal("A", false));

        solver.add(f.parse("~A"));
        solver.sat();
        assertThat(solver.unsatCore()).isNotNull();
    }

    @Test
    public void testCoreAndAssumptions3() throws ParserException {
        // Unit test for DRUP issue which led to java.lang.ArrayIndexOutOfBoundsException: -1
        final FormulaFactory f = new FormulaFactory();
        final MiniSat solver = MiniSat.miniSat(f, MiniSatConfig.builder().proofGeneration(true).cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());

        solver.add(f.parse("X => Y"));
        solver.add(f.parse("X => Z"));
        solver.add(f.parse("C => E"));
        solver.add(f.parse("D => ~F"));
        solver.add(f.parse("B => M"));
        solver.add(f.parse("D => N"));
        solver.add(f.parse("G => O"));
        solver.add(f.parse("A => B"));
        solver.add(f.parse("T1 <=> A & K & ~B & ~C"));
        solver.add(f.parse("T2 <=> A & B & C & K"));
        solver.add(f.parse("T1 + T2 = 1"));
        solver.sat(); // required for DRUP issue

        solver.add(f.parse("Y => ~X & D"));
        solver.add(f.parse("X"));

        solver.sat();
        assertThat(solver.unsatCore()).isNotNull();
    }

    @Test
    public void testWithCcPropositions() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final SATSolver solver = MiniSat.miniSat(f, MiniSatConfig.builder().proofGeneration(true).cnfMethod(MiniSatConfig.CNFMethod.PG_ON_SOLVER).build());
        final ExtendedProposition<StringBackpack> p1 = new ExtendedProposition<>(new StringBackpack("CC"), f.parse("A + B + C <= 1"));
        final StandardProposition p2 = new StandardProposition(f.parse("A"));
        final StandardProposition p3 = new StandardProposition(f.parse("B"));
        final StandardProposition p4 = new StandardProposition(f.parse("X & Y"));
        solver.add(p1);
        solver.add(p2);
        solver.add(p3);
        solver.add(p4);
        assertThat(solver.sat()).isEqualTo(FALSE);
        assertThat(solver.unsatCore().propositions()).containsExactlyInAnyOrder(p1, p2, p3);
    }

    @Test
    public void testWithSpecialUnitCaseMiniSat() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final SATSolver solver = MiniSat.miniSat(f, MiniSatConfig.builder().proofGeneration(true).build());
        final StandardProposition p1 = new StandardProposition(f.parse("a => b"));
        final StandardProposition p2 = new StandardProposition(f.parse("a => c | d"));
        final StandardProposition p3 = new StandardProposition(f.parse("b => c | d"));
        final StandardProposition p4 = new StandardProposition(f.parse("e | f | g | h => i"));
        final StandardProposition p5 = new StandardProposition(f.parse("~j => k | j"));
        final StandardProposition p6 = new StandardProposition(f.parse("b => ~(e | f)"));
        final StandardProposition p7 = new StandardProposition(f.parse("c => ~j"));
        final StandardProposition p8 = new StandardProposition(f.parse("l | m => ~i"));
        final StandardProposition p9 = new StandardProposition(f.parse("j => (f + g + h = 1)"));
        final StandardProposition p10 = new StandardProposition(f.parse("d => (l + m + e + f = 1)"));
        final StandardProposition p11 = new StandardProposition(f.parse("~k"));
        solver.addPropositions(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11);
        assertThat(solver.sat()).isEqualTo(TRUE);
        solver.add(f.variable("a"));
        assertThat(solver.sat()).isEqualTo(FALSE);
        assertThat(solver.unsatCore().propositions()).contains(p1, p2, p4, p5, p6, p7, p8, p9, p10, p11);
    }

    @Test
    public void testWithSpecialUnitCaseGlucose() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final SATSolver solver = MiniSat.glucose(f, MiniSatConfig.builder().proofGeneration(true).incremental(false).build(), GlucoseConfig.builder().build());
        final StandardProposition p1 = new StandardProposition(f.parse("a => b"));
        final StandardProposition p2 = new StandardProposition(f.parse("a => c | d"));
        final StandardProposition p3 = new StandardProposition(f.parse("b => c | d"));
        final StandardProposition p4 = new StandardProposition(f.parse("e | f | g | h => i"));
        final StandardProposition p5 = new StandardProposition(f.parse("~j => k | j"));
        final StandardProposition p6 = new StandardProposition(f.parse("b => ~(e | f)"));
        final StandardProposition p7 = new StandardProposition(f.parse("c => ~j"));
        final StandardProposition p8 = new StandardProposition(f.parse("l | m => ~i"));
        final StandardProposition p9 = new StandardProposition(f.parse("j => (f + g + h = 1)"));
        final StandardProposition p10 = new StandardProposition(f.parse("d => (l + m + e + f = 1)"));
        final StandardProposition p11 = new StandardProposition(f.parse("~k"));
        solver.addPropositions(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11);
        assertThat(solver.sat()).isEqualTo(TRUE);
        solver.add(f.variable("a"));
        assertThat(solver.sat()).isEqualTo(FALSE);
        assertThat(solver.unsatCore().propositions()).contains(p1, p2, p4, p5, p6, p7, p8, p9, p10, p11);
    }

    /**
     * Checks that each formula of the core is part of the original problem and that the core is really unsat.
     * @param originalCore the original core
     * @param cnf          the original problem
     */
    private void verifyCore(final UNSATCore<Proposition> originalCore, final List<Formula> cnf) {
        final List<Formula> core = new ArrayList<>(originalCore.propositions().size());
        for (final Proposition prop : originalCore.propositions()) {
            core.add(prop.formula());
        }
        final SoftAssertions softly = new SoftAssertions();
        softly.assertThat(cnf).as("Core contains only original clauses").containsAll(core);
        final MiniSat solver = MiniSat.glucose(this.f,
                MiniSatConfig.builder().proofGeneration(true).incremental(false).build(),
                GlucoseConfig.builder().build());
        solver.add(core);
        softly.assertThat(solver.sat()).as("Core is unsatisfiable").isEqualTo(Tristate.FALSE);
        softly.assertAll();
    }

    private static final class StringBackpack implements PropositionBackpack {
        private final String string;

        private StringBackpack(final String string) {
            this.string = string;
        }
    }
}
