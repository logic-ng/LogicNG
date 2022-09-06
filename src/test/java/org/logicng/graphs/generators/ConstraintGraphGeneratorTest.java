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

package org.logicng.graphs.generators;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.logicng.cardinalityconstraints.CCConfig;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;
import org.logicng.graphs.algorithms.ConnectedComponentsComputation;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.Node;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.io.parsers.PseudoBooleanParser;
import org.logicng.io.readers.FormulaReader;
import org.logicng.transformations.cnf.CNFFactorization;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Unit tests for {@link ConstraintGraphGenerator}.
 * @version 2.0.0
 * @since 2.0.0
 */
public class ConstraintGraphGeneratorTest {

    @Test
    public void testSimple() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        assertThat(ConstraintGraphGenerator.generateFromCnf(f.falsum()).nodes()).isEmpty();
        assertThat(ConstraintGraphGenerator.generateFromCnf(f.verum()).nodes()).isEmpty();
        Graph<Variable> graph = ConstraintGraphGenerator.generateFromCnf(p.parse("a"));
        assertThat(graph.nodes()).containsExactly(graph.node(f.variable("a")));
        graph = ConstraintGraphGenerator.generateFromCnf(p.parse("~a"));
        assertThat(graph.nodes()).containsExactly(graph.node(f.variable("a")));
    }

    @Test
    public void testOr() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final Graph<Variable> expected = new Graph<>();
        final Node<Variable> a = expected.node(f.variable("a"));
        final Node<Variable> b = expected.node(f.variable("b"));
        final Node<Variable> c = expected.node(f.variable("c"));
        expected.connect(a, b);
        expected.connect(a, c);
        expected.connect(b, c);
        assertThat(ConstraintGraphGenerator.generateFromCnf(p.parse("a | ~b | c")).toString()).isEqualTo(expected.toString());
    }

    @Test
    public void testCC() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Graph<Variable> expected = new Graph<>();
        final Node<Variable> a = expected.node(f.variable("a"));
        final Node<Variable> b = expected.node(f.variable("b"));
        final Node<Variable> c = expected.node(f.variable("c"));
        expected.connect(a, b);
        expected.connect(a, c);
        expected.connect(b, c);
        assertThat(ConstraintGraphGenerator.generateFromCnf(p.parse("a + b + c <= 1")).toString()).isEqualTo(expected.toString());
    }

    @Test
    public void testCnf() throws ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PseudoBooleanParser p = new PseudoBooleanParser(f);
        final Graph<Variable> expected = new Graph<>();
        final Node<Variable> a = expected.node(f.variable("a"));
        final Node<Variable> b = expected.node(f.variable("b"));
        final Node<Variable> c = expected.node(f.variable("c"));
        final Node<Variable> d = expected.node(f.variable("d"));
        final Node<Variable> e = expected.node(f.variable("e"));
        expected.node(f.variable("g"));
        expected.connect(a, b);
        expected.connect(a, c);
        expected.connect(b, c);
        expected.connect(d, a);
        expected.connect(d, e);
        assertThat(ConstraintGraphGenerator.generateFromCnf(p.parse("(a | ~b | c) & (d | ~a) & (d + e = 1) & g")).toString()).isEqualTo(expected.toString());
    }

    @Test
    public void testRealExample() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        f.putConfiguration(CCConfig.builder().amoEncoding(CCConfig.AMO_ENCODER.PURE).build());
        final Formula parsed = FormulaReader.readPseudoBooleanFormula("src/test/resources/formulas/formula1.txt", f);
        final List<Formula> formulas = new ArrayList<>();
        for (final Formula formula : parsed) {
            if (formula instanceof PBConstraint) {
                formulas.add(formula);
            } else {
                formulas.add(formula.transform(new CNFFactorization()));
            }
        }
        final Graph<Variable> constraintGraph = ConstraintGraphGenerator.generateFromCnf(formulas);
        final Set<Set<Node<Variable>>> ccs = ConnectedComponentsComputation.compute(constraintGraph);
        assertThat(ccs).hasSize(4);
    }
}
