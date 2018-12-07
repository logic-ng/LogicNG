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
//  Copyright 2015-2018 Christoph Zengler                                //
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
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.MiniSat;
import org.logicng.testutils.PigeonHoleGenerator;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Unit tests for the class {@link MUSGeneration}.
 * @version 1.3
 * @since 1.1
 */
public class MUSGenerationTest {

  private final FormulaFactory f = new FormulaFactory();
  private final PigeonHoleGenerator pg = new PigeonHoleGenerator(f);

  private final List<StandardProposition> pg3;
  private final List<StandardProposition> pg4;
  private final List<StandardProposition> pg5;
  private final List<StandardProposition> pg6;
  private final List<StandardProposition> pg7;
  private final List<StandardProposition> file1;
  private final List<StandardProposition> file2;
  private final List<StandardProposition> file3;
  private final List<StandardProposition> file4;

  public MUSGenerationTest() throws IOException {
    this.pg3 = generatePGPropositions(3);
    this.pg4 = generatePGPropositions(4);
    this.pg5 = generatePGPropositions(5);
    this.pg6 = generatePGPropositions(6);
    this.pg7 = generatePGPropositions(7);
    this.file1 = readDimacs("src/test/resources/sat/3col40_5_10.shuffled.cnf");
    this.file2 = readDimacs("src/test/resources/sat/x1_16.shuffled.cnf");
    this.file3 = readDimacs("src/test/resources/sat/grid_10_20.shuffled.cnf");
    this.file4 = readDimacs("src/test/resources/sat/ca032.shuffled.cnf");
  }

  @Test
  public void testDeletionBasedMUS() {
    final MUSGeneration mus = new MUSGeneration();
    final UNSATCore<StandardProposition> mus1 = mus.computeMUS(this.pg3, this.f);
    final UNSATCore<StandardProposition> mus2 = mus.computeMUS(this.pg4, this.f);
    final UNSATCore<StandardProposition> mus3 = mus.computeMUS(this.pg5, this.f);
    final UNSATCore<StandardProposition> mus4 = mus.computeMUS(this.pg6, this.f);
    final UNSATCore<StandardProposition> mus5 = mus.computeMUS(this.pg7, this.f);
    final UNSATCore<StandardProposition> mus6 = mus.computeMUS(this.file1, this.f);
    final UNSATCore<StandardProposition> mus7 = mus.computeMUS(this.file2, this.f);
    final UNSATCore<StandardProposition> mus8 = mus.computeMUS(this.file3, this.f);
    final UNSATCore<StandardProposition> mus9 = mus.computeMUS(this.file4, this.f);
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

  @Test
  public void testPlainInsertionBasedMUS() {
    final MUSGeneration mus = new MUSGeneration();
    final MUSConfig config = new MUSConfig.Builder().algorithm(MUSConfig.Algorithm.PLAIN_INSERTION).build();
    final UNSATCore<StandardProposition> mus1 = mus.computeMUS(this.pg3, this.f, config);
    final UNSATCore<StandardProposition> mus2 = mus.computeMUS(this.pg4, this.f, config);
    final UNSATCore<StandardProposition> mus3 = mus.computeMUS(this.pg5, this.f, config);
    final UNSATCore<StandardProposition> mus6 = mus.computeMUS(this.file1, this.f, config);
    final UNSATCore<StandardProposition> mus7 = mus.computeMUS(this.file2, this.f, config);
    testMUS(pg3, mus1);
    testMUS(pg4, mus2);
    testMUS(pg5, mus3);
    testMUS(file1, mus6);
    testMUS(file2, mus7);
  }

  @Test
  public void testToString() {
    final MUSGeneration mus = new MUSGeneration();
    Assert.assertEquals("MUSGeneration", mus.toString());
  }

  private List<StandardProposition> generatePGPropositions(int n) {
    final List<StandardProposition> result = new ArrayList<>();
    final Formula pgf = pg.generate(n);
    for (final Formula f : pgf)
      result.add(new StandardProposition(f));
    return result;
  }

  private List<StandardProposition> readDimacs(final String fileName) throws IOException {
    final List<StandardProposition> result = new ArrayList<>();
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

  private void testMUS(final List<StandardProposition> original, final UNSATCore<StandardProposition> mus) {
    Assert.assertTrue(mus.isMUS());
    Assert.assertTrue(mus.propositions().size() <= original.size());
    final MiniSat miniSat = MiniSat.miniSat(this.f);
    for (final StandardProposition p : mus.propositions()) {
      Assert.assertTrue(original.contains(p));
      Assert.assertEquals(Tristate.TRUE, miniSat.sat());
      miniSat.add(p);
    }
    Assert.assertEquals(Tristate.FALSE, miniSat.sat());
  }
}
