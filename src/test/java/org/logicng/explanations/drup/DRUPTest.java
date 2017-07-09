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
 * @version 1.2
 * @since 1.2
 */
public class DRUPTest {

  final FormulaFactory f = new FormulaFactory();

  private final SATSolver[] solvers;

  public DRUPTest() {
    this.solvers = new SATSolver[3];
    this.solvers[0] = MiniSat.miniSat(f, new MiniSatConfig.Builder().proofGeneration(true).incremental(true).build());
    this.solvers[1] = MiniSat.miniSat(f, new MiniSatConfig.Builder().proofGeneration(true).incremental(false).build());
    this.solvers[2] = MiniSat.glucose(f, new MiniSatConfig.Builder().proofGeneration(true).incremental(false).build(),
            new GlucoseConfig.Builder().build());
  }

  @Test
  public void testUnsatCoresFromDimacs() throws IOException, ParserException {
    final List<List<Formula>> cnfs = new ArrayList<>(3);
    cnfs.add(DimacsReader.readCNF("tests/drup/simple_input.cnf", f));
    cnfs.add(DimacsReader.readCNF("tests/drup/pg4_input.cnf", f));
    cnfs.add(DimacsReader.readCNF("tests/drup/avg_input.cnf", f));

    for (SATSolver solver : this.solvers) {
      for (List<Formula> cnf : cnfs) {
        solver.add(cnf);
        assertThat(solver.sat()).isEqualTo(Tristate.FALSE);
        final UNSATCore unsatCore = solver.unsatCore();
        verifyCore(unsatCore, cnf);
        solver.reset();
      }
    }
  }

  @Test
  public void testUnsatCoresFromLargeTestset() throws IOException, ParserException {
    final File testFolder = new File("tests/sat");
    final File[] files = testFolder.listFiles();
    assert files != null;
    for (final SATSolver solver : this.solvers) {
      for (final File file : files) {
        final String fileName = file.getName();
        if (fileName.endsWith(".cnf")) {
          List<Formula> cnf = DimacsReader.readCNF(file, f);
          solver.add(cnf);
          if (solver.sat() == FALSE) {
            final UNSATCore unsatCore = solver.unsatCore();
            verifyCore(unsatCore, cnf);
          }
          solver.reset();
        }
      }
      solver.reset();
    }
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
      final UNSATCore unsatCore = solver.unsatCore();
      assertThat(unsatCore.propositions()).containsExactlyInAnyOrder(propositions.get(0), propositions.get(1),
              propositions.get(2), propositions.get(3), propositions.get(4), propositions.get(5));
      System.out.println(unsatCore);
      solver.reset();
    }
  }

  /**
   * Checks that each formula of the core is part of the original problem and that the core is really unsat.
   * @param originalCore the original core
   * @param cnf          the original problem
   */
  private void verifyCore(final UNSATCore originalCore, final List<Formula> cnf) {
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
