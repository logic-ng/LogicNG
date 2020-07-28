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

package org.logicng.graphs.algorithms;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.logicng.cardinalityconstraints.CCConfig;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.GraphTest;
import org.logicng.graphs.datastructures.Node;
import org.logicng.graphs.generators.ConstraintGraphGenerator;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.readers.FormulaReader;
import org.logicng.transformations.cnf.CNFFactorization;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Unit tests for the class {@link ConnectedComponentsComputation}.
 * @version 2.0.0
 * @since 1.2
 */
public class ConnectedComponentsComputerTest {

    @Test
    public void graph30Test() throws IOException {
        final Graph<Long> g = GraphTest.getLongGraph("30");
        for (long i = 0; i < 30; i++) {
            g.node(i);
        }

        assertThat(g.nodes().size()).isEqualTo(30);

        final Set<Set<Node<Long>>> ccs = ConnectedComponentsComputation.compute(g);
        assertThat(ccs.size()).isEqualTo(7);
        int bigOnes = 0;
        for (final Set<Node<Long>> cc : ccs) {
            if (cc.size() > 1) {
                assertThat(bigOnes < 4).isTrue();
                bigOnes++;
                assertThat(cc.size() > 4).isTrue();
            } else {
                assertThat(cc.size()).isEqualTo(1);
            }
            int equals = 0;
            for (final Set<Node<Long>> cc2 : ccs) {
                final Set<Node<Long>> cut = new HashSet<>(cc2);
                cut.retainAll(cc);
                if (cut.size() == cc.size()) {
                    equals++;
                } else {
                    assertThat(cut.isEmpty()).isTrue();
                }
            }
            assertThat(equals).isEqualTo(1);
        }
    }

    @Test
    public void graph60Test() throws IOException {
        final Graph<Long> g = GraphTest.getLongGraph("50");
        for (long i = 0; i < 60; i++) {
            g.node(i);
        }

        assertThat(g.nodes().size()).isEqualTo(60);

        final Set<Set<Node<Long>>> ccs = ConnectedComponentsComputation.compute(g);
        assertThat(ccs.size()).isEqualTo(11);
        boolean bigOneFound = false;
        for (final Set<Node<Long>> cc : ccs) {
            if (cc.size() > 1) {
                assertThat(bigOneFound).isFalse();
                bigOneFound = true;
                assertThat(cc.size()).isEqualTo(50);
            } else {
                assertThat(cc.size()).isEqualTo(1);
            }
            int equals = 0;
            for (final Set<Node<Long>> cc2 : ccs) {
                final Set<Node<Long>> cut = new HashSet<>(cc2);
                cut.retainAll(cc);
                if (cut.size() == cc.size()) {
                    equals++;
                } else {
                    assertThat(cut.isEmpty()).isTrue();
                }
            }
            assertThat(equals).isEqualTo(1);
        }
    }

    @Test
    public void testFormulaSplit() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        f.putConfiguration(CCConfig.builder().amoEncoding(CCConfig.AMO_ENCODER.PURE).build());
        final Formula parsed = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/formula1.txt", f);
        final List<Formula> formulas = new ArrayList<>();
        final List<Formula> originalFormulas = new ArrayList<>();
        for (final Formula formula : parsed) {
            originalFormulas.add(formula);
            if (formula instanceof PBConstraint) {
                formulas.add(formula);
            } else {
                formulas.add(formula.transform(new CNFFactorization()));
            }
        }
        final Graph<Variable> constraintGraph = ConstraintGraphGenerator.generateFromCnf(formulas);
        final Set<Set<Node<Variable>>> ccs = ConnectedComponentsComputation.compute(constraintGraph);
        final List<List<Formula>> split = ConnectedComponentsComputation.splitFormulasByComponent(originalFormulas, ccs);
        assertThat(split).hasSize(4);
        assertThat(split.get(0)).hasSize(1899);
        assertThat(split.get(1)).hasSize(3);
        assertThat(split.get(2)).hasSize(3);
        assertThat(split.get(3)).hasSize(3);
    }

    @Test
    public void testFormulaSplitIllegal() {
        final FormulaFactory f = new FormulaFactory();
        final Graph<Variable> graph = ConstraintGraphGenerator.generateFromCnf(f.variable("B"));
        final Set<Set<Node<Variable>>> ccs = Collections.singleton(Collections.singleton(graph.node(f.variable("B"))));
        assertThatThrownBy(() -> ConnectedComponentsComputation.splitFormulasByComponent(Collections.singletonList(f.variable("A")), ccs))
                .isInstanceOf(IllegalArgumentException.class);
    }
}
