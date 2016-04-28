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
//  Copyright 2015-2016 Christoph Zengler                                //
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

package org.logicng.explanations.unsatcores;

import org.junit.Assert;
import org.junit.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.propositions.Proposition;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.sat.PigeonHoleGenerator;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Unit tests for the class {@link MUSGeneration}.
 * @version 1.1
 * @since 1.1
 */
public class MUSGenerationTest {

  private final FormulaFactory f = new FormulaFactory();
  private final PigeonHoleGenerator pg = new PigeonHoleGenerator(f);

  private final List<Proposition> pg3;
  private final List<Proposition> pg4;
  private final List<Proposition> pg5;
  private final List<Proposition> pg6;
  private final List<Proposition> pg7;
  private final List<Proposition> file1;
  private final List<Proposition> file2;
  private final List<Proposition> file3;
  private final List<Proposition> file4;

  public MUSGenerationTest() throws IOException {
    this.pg3 = generatePGPropositions(3);
    this.pg4 = generatePGPropositions(4);
    this.pg5 = generatePGPropositions(5);
    this.pg6 = generatePGPropositions(6);
    this.pg7 = generatePGPropositions(7);
    this.file1 = readDimacs("tests/sat/3col40_5_10.shuffled.cnf");
    this.file2 = readDimacs("tests/sat/x1_16.shuffled.cnf");
    this.file3 = readDimacs("tests/sat/grid_10_20.shuffled.cnf");
    this.file4 = readDimacs("tests/sat/ca032.shuffled.cnf");
  }

  @Test
  public void testDeletionBasedMUS() throws IOException {
    final MUSGeneration mus = new MUSGeneration();
    final long start = System.currentTimeMillis();
    final UNSATCore mus1 = mus.computeMUS(this.pg3, this.f);
    final UNSATCore mus2 = mus.computeMUS(this.pg4, this.f);
    final UNSATCore mus3 = mus.computeMUS(this.pg5, this.f);
    final UNSATCore mus4 = mus.computeMUS(this.pg6, this.f);
    final UNSATCore mus5 = mus.computeMUS(this.pg7, this.f);
    final UNSATCore mus6 = mus.computeMUS(this.file1, this.f);
    final UNSATCore mus7 = mus.computeMUS(this.file2, this.f);
    final UNSATCore mus8 = mus.computeMUS(this.file3, this.f);
    final UNSATCore mus9 = mus.computeMUS(this.file4, this.f);
    final long stop = System.currentTimeMillis();
    System.out.println("Deletion Based MUS: " + (stop - start) + " ms.");
    testMUS(pg3, mus1);
    testMUS(pg4, mus2);
    testMUS(pg5, mus3);
    testMUS(pg6, mus4);
    testMUS(pg7, mus5);
    testMUS(file1, mus6);
    testMUS(file2, mus7);
    testMUS(file3, mus8);
    testMUS(file4, mus9);
  }

  private List<Proposition> generatePGPropositions(int n) {
    final List<Proposition> result = new ArrayList<>();
    final Formula pgf = pg.generate(n);
    for (final Formula f : pgf)
      result.add(new StandardProposition(f));
    return result;
  }

  private List<Proposition> readDimacs(final String fileName) throws IOException {
    final List<Proposition> result = new ArrayList<>();
    final BufferedReader reader = new BufferedReader(new FileReader(fileName));
    while (reader.ready()) {
      final String line = reader.readLine();
      if (!line.startsWith("p") && !line.startsWith("c")) {
        final String[] tokens = line.split("\\s");
        final List<Literal> clause = new ArrayList<>();
        for (int i = 0; i < tokens.length - 1; i++) {
          int lit = Integer.parseInt(tokens[i]);
          clause.add(lit < 0 ? f.literal("v" + (-lit), false) : f.literal("v" + lit, true));
        }
        result.add(new StandardProposition(f.clause(clause)));
      }
    }
    return result;
  }

  private void testMUS(final List<Proposition> original, final UNSATCore mus) {
    Assert.assertTrue(mus.isMUS());
    Assert.assertTrue(mus.propositions().size() <= original.size());
    final MiniSat miniSat = MiniSat.miniSat(this.f);
    for (final Proposition p : mus.propositions()) {
      Assert.assertTrue(original.contains(p));
      Assert.assertEquals(Tristate.TRUE, miniSat.sat());
      miniSat.add(p);
    }
    Assert.assertEquals(Tristate.FALSE, miniSat.sat());
  }
}
