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

import org.assertj.core.api.SoftAssertions;
import org.junit.Test;
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.unsatcores.UNSATCore;
import org.logicng.explanations.unsatcores.drup.DRUPTrim;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.readers.DimacsReader;
import org.logicng.propositions.Proposition;
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.logicng.datastructures.Tristate.FALSE;

/**
 * Unit tests for {@link DRUPTrim}.
 * @version 1.3
 * @since 1.3
 */
public class DRUPTest {

  private final FormulaFactory f = new FormulaFactory();

  private final SATSolver[] solvers;

  public DRUPTest() {
    this.solvers = new SATSolver[3];
    this.solvers[0] = MiniSat.miniSat(f, new MiniSatConfig.Builder().proofGeneration(true).incremental(true).build());
    this.solvers[1] = MiniSat.miniSat(f, new MiniSatConfig.Builder().proofGeneration(true).incremental(false).build());
    this.solvers[2] = MiniSat.glucose(f, new MiniSatConfig.Builder().proofGeneration(true).incremental(false).build(),
            new GlucoseConfig.Builder().build());
  }

  @Test
  public void testUnsatCoresFromDimacs() throws IOException {
    final List<List<Formula>> cnfs = new ArrayList<>(3);
    cnfs.add(DimacsReader.readCNF("src/test/resources/drup/simple_input.cnf", f));
    cnfs.add(DimacsReader.readCNF("src/test/resources/drup/pg4_input.cnf", f));
    cnfs.add(DimacsReader.readCNF("src/test/resources/drup/avg_input.cnf", f, "var"));

    for (SATSolver solver : this.solvers) {
      for (List<Formula> cnf : cnfs) {
        solver.add(cnf);
        assertThat(solver.sat()).isEqualTo(Tristate.FALSE);
        final UNSATCore<Proposition> unsatCore = solver.unsatCore();
        verifyCore(unsatCore, cnf);
        solver.reset();
      }
    }
  }

  @Test
  public void testUnsatCoresFromLargeTestset() throws IOException {
    final File testFolder = new File("src/test/resources/sat");
    final File[] files = testFolder.listFiles();
    assert files != null;
    int count = 0;
    for (final SATSolver solver : this.solvers) {
      for (final File file : files) {
        final String fileName = file.getName();
        if (fileName.endsWith(".cnf")) {
          List<Formula> cnf = DimacsReader.readCNF(file, f);
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
          List<Formula> cnf = DimacsReader.readCNF(file, f);
          solver.add(cnf);
          assertThat(solver.sat()).isEqualTo(Tristate.FALSE);
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
    final PropositionalParser p = new PropositionalParser(f);
    final List<Proposition> propositions = new ArrayList<>();
    propositions.add(new StandardProposition("P1", f.parse("((a & b) => c) &  ((a & b) => d)")));
    propositions.add(new StandardProposition("P2", f.parse("(c & d) <=> ~e")));
    propositions.add(new StandardProposition("P3", new ImmutableFormulaList(p.parse("~e => f | g"))));
    propositions.add(new StandardProposition("P4", f.parse("f => ~a"), f.parse("g => ~b"), f.parse("p & q")));
    propositions.add(new StandardProposition("P5", f.parse("a => b")));
    propositions.add(new StandardProposition("P6", f.parse("a")));
    propositions.add(new StandardProposition("P7", f.parse("g | h")));
    propositions.add(new StandardProposition("P8", f.parse("x => ~y | z"), f.parse("z | w")));

    for (final SATSolver solver : this.solvers) {
      for (Proposition proposition : propositions)
        solver.add(proposition);
      assertThat(solver.sat()).isEqualTo(FALSE);
      final UNSATCore<Proposition> unsatCore = solver.unsatCore();
      assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(propositions.get(0), propositions.get(1),
              propositions.get(2), propositions.get(3), propositions.get(4), propositions.get(5));
      solver.reset();
    }
  }

  @Test
  public void testPropositionIncDec() throws ParserException {
    final PropositionalParser p = new PropositionalParser(f);
    SATSolver solver = this.solvers[0];
    final StandardProposition p1 = new StandardProposition("P1", f.parse("((a & b) => c) &  ((a & b) => d)"));
    final StandardProposition p2 = new StandardProposition("P2", f.parse("(c & d) <=> ~e"));
    final StandardProposition p3 = new StandardProposition("P3", new ImmutableFormulaList(p.parse("~e => f | g")));
    final StandardProposition p4 = new StandardProposition("P4", f.parse("f => ~a"), f.parse("g => ~b"), f.parse("p & q"));
    final StandardProposition p5 = new StandardProposition("P5", f.parse("a => b"));
    final StandardProposition p6 = new StandardProposition("P6", f.parse("a"));
    final StandardProposition p7 = new StandardProposition("P7", f.parse("g | h"));
    final StandardProposition p8 = new StandardProposition("P8", f.parse("x => ~y | z"), f.parse("z | w"));

    final StandardProposition p9 = new StandardProposition("P9", f.parse("a"), f.parse("b"));

    final StandardProposition p10 = new StandardProposition("P10", f.parse("p => q"), f.parse("p"));
    final StandardProposition p11 = new StandardProposition("P11", f.parse("a"), f.parse("~q"));

    solver.add(p1);
    solver.add(p2);
    solver.add(p3);
    solver.add(p4);
    final SolverState state1 = solver.saveState();
    solver.add(p5);
    solver.add(p6);
    final SolverState state2 = solver.saveState();
    solver.add(p7);
    solver.add(p8);

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
      assertThat(solver.sat()).isEqualTo(Tristate.TRUE);
      final StandardProposition p1 = new StandardProposition("P1", f.parse("$false"));
      solver.add(p1);
      assertThat(solver.sat()).isEqualTo(Tristate.FALSE);
      UNSATCore<Proposition> unsatCore = solver.unsatCore();
      assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(p1);

      solver.reset();
      assertThat(solver.sat()).isEqualTo(Tristate.TRUE);
      final StandardProposition p2 = new StandardProposition("P2", f.parse("a"));
      solver.add(p2);
      assertThat(solver.sat()).isEqualTo(Tristate.TRUE);
      final StandardProposition p3 = new StandardProposition("P3", f.parse("~a"));
      solver.add(p3);
      assertThat(solver.sat()).isEqualTo(Tristate.FALSE);
      unsatCore = solver.unsatCore();
      assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(p2, p3);
    }
  }

  /**
   * Checks that each formula of the core is part of the original problem and that the core is really unsat.
   * @param originalCore the original core
   * @param cnf          the original problem
   */
  private void verifyCore(final UNSATCore<Proposition> originalCore, final List<Formula> cnf) {
    final List<Formula> core = new ArrayList<>(originalCore.propositions().size());
    for (Proposition prop : originalCore.propositions())
      core.add(prop.formula(f));
    final SoftAssertions softly = new SoftAssertions();
    softly.assertThat(cnf).as("Core contains only original clauses").containsAll(core);
    MiniSat solver = MiniSat.glucose(f,
            new MiniSatConfig.Builder().proofGeneration(true).incremental(false).build(),
            new GlucoseConfig.Builder().build());
    solver.add(core);
    softly.assertThat(solver.sat()).as("Core is unsatisfiable").isEqualTo(Tristate.FALSE);
    softly.assertAll();
  }
}
