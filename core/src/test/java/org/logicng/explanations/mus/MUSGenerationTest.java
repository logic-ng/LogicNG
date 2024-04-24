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

package org.logicng.explanations.mus;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.UNSATCore;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.handlers.BoundedSatHandler;
import org.logicng.handlers.SATHandler;
import org.logicng.io.readers.DimacsReader;
import org.logicng.propositions.StandardProposition;
import org.logicng.solvers.MiniSat;
import org.logicng.testutils.PigeonHoleGenerator;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Unit tests for the class {@link MUSGeneration}.
 * @version 2.1.0
 * @since 1.1
 */
public class MUSGenerationTest {

    private final FormulaFactory f = new FormulaFactory();
    private final PigeonHoleGenerator pg = new PigeonHoleGenerator(this.f);

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
    public void testNoFormulas() {
        final MUSGeneration mus = new MUSGeneration();
        assertThatThrownBy(() -> mus.computeMUS(Collections.emptyList(), this.f, MUSConfig.builder().build())).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testSATFormulaSetDeletionBasedMUS() {
        final MUSGeneration mus = new MUSGeneration();
        final StandardProposition proposition = new StandardProposition(this.f.variable("a"));
        assertThatThrownBy(() -> mus.computeMUS(Collections.singletonList(proposition), this.f,
                MUSConfig.builder().algorithm(MUSConfig.Algorithm.DELETION).build())).isInstanceOf(IllegalArgumentException.class);
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
        testMUS(this.pg3, mus1);
        testMUS(this.pg4, mus2);
        testMUS(this.pg5, mus3);
        testMUS(this.pg6, mus4);
        testMUS(this.pg7, mus5);
        testMUS(this.file1, mus6);
        testMUS(this.file2, mus7);
        testMUS(this.file3, mus8);
        testMUS(this.file4, mus9);
    }

    @Test
    public void testSATFormulaSetPlainInsertionBasedMUS() {
        final MUSGeneration mus = new MUSGeneration();
        final StandardProposition proposition = new StandardProposition(this.f.variable("a"));
        assertThatThrownBy(() -> mus.computeMUS(Collections.singletonList(proposition), this.f,
                MUSConfig.builder().algorithm(MUSConfig.Algorithm.PLAIN_INSERTION).build())).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void testPlainInsertionBasedMUS() {
        final MUSGeneration mus = new MUSGeneration();
        final MUSConfig config = MUSConfig.builder().algorithm(MUSConfig.Algorithm.PLAIN_INSERTION).build();
        final UNSATCore<StandardProposition> mus1 = mus.computeMUS(this.pg3, this.f, config);
        final UNSATCore<StandardProposition> mus2 = mus.computeMUS(this.pg4, this.f, config);
        final UNSATCore<StandardProposition> mus3 = mus.computeMUS(this.pg5, this.f, config);
        final UNSATCore<StandardProposition> mus6 = mus.computeMUS(this.file1, this.f, config);
        final UNSATCore<StandardProposition> mus7 = mus.computeMUS(this.file2, this.f, config);
        testMUS(this.pg3, mus1);
        testMUS(this.pg4, mus2);
        testMUS(this.pg5, mus3);
        testMUS(this.file1, mus6);
        testMUS(this.file2, mus7);
    }

    @Test
    public void testDeletionBasedCancellationPoints() throws IOException {
        final MUSGeneration mus = new MUSGeneration();
        final List<StandardProposition> propositions = DimacsReader.readCNF("src/test/resources/sat/too_large_gr_rcs_w5.shuffled.cnf", f).stream()
                .map(StandardProposition::new)
                .collect(Collectors.toList());
        for (int numStarts = 0; numStarts < 20; numStarts++) {
            final SATHandler handler = new BoundedSatHandler(numStarts);
            final MUSConfig config = MUSConfig.builder().handler(handler).algorithm(MUSConfig.Algorithm.PLAIN_INSERTION).build();

            final UNSATCore<StandardProposition> result = mus.computeMUS(propositions, f, config);

            assertThat(handler.aborted()).isTrue();
            assertThat(result).isNull();
        }
    }

    @Test
    public void testCancellationPoints() throws IOException {
        final MUSGeneration mus = new MUSGeneration();
        final List<StandardProposition> propositions = DimacsReader.readCNF("src/test/resources/sat/unsat/bf0432-007.cnf", f).stream()
                .map(StandardProposition::new)
                .collect(Collectors.toList());
        final List<MUSConfig.Algorithm> algorithms = Arrays.asList(MUSConfig.Algorithm.DELETION, MUSConfig.Algorithm.PLAIN_INSERTION);
        for (final MUSConfig.Algorithm algorithm : algorithms) {
            for (int numStarts = 0; numStarts < 10; numStarts++) {
                final SATHandler handler = new BoundedSatHandler(numStarts);
                final MUSConfig config = MUSConfig.builder().handler(handler).algorithm(algorithm).build();

                final UNSATCore<StandardProposition> result = mus.computeMUS(propositions, f, config);

                assertThat(handler.aborted()).isTrue();
                assertThat(result).isNull();
            }
        }
    }

    @Test
    public void testToString() {
        final MUSGeneration mus = new MUSGeneration();
        assertThat(mus.toString()).isEqualTo("MUSGeneration");
    }

    private List<StandardProposition> generatePGPropositions(final int n) {
        final List<StandardProposition> result = new ArrayList<>();
        final Formula pgf = this.pg.generate(n);
        for (final Formula f : pgf) {
            result.add(new StandardProposition(f));
        }
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
                    final int lit = Integer.parseInt(tokens[i]);
                    clause.add(lit < 0 ? this.f.literal("v" + (-lit), false) : this.f.literal("v" + lit, true));
                }
                result.add(new StandardProposition(this.f.clause(clause)));
            }
        }
        return result;
    }

    private void testMUS(final List<StandardProposition> original, final UNSATCore<StandardProposition> mus) {
        assertThat(mus.isMUS()).isTrue();
        assertThat(mus.propositions().size() <= original.size()).isTrue();
        final MiniSat miniSat = MiniSat.miniSat(this.f);
        for (final StandardProposition p : mus.propositions()) {
            assertThat(original.contains(p)).isTrue();
            assertThat(miniSat.sat()).isEqualTo(Tristate.TRUE);
            miniSat.add(p);
        }
        assertThat(miniSat.sat()).isEqualTo(Tristate.FALSE);
    }
}
