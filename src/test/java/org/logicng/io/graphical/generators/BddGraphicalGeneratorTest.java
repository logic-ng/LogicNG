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

package org.logicng.io.graphical.generators;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.contentOf;
import static org.logicng.io.graphical.GraphicalColor.BLACK;
import static org.logicng.io.graphical.GraphicalColor.CYAN;
import static org.logicng.io.graphical.GraphicalColor.GRAY_LIGHT;
import static org.logicng.io.graphical.GraphicalColor.GREEN;
import static org.logicng.io.graphical.GraphicalColor.ORANGE;
import static org.logicng.io.graphical.GraphicalColor.PURPLE;
import static org.logicng.io.graphical.GraphicalColor.RED;
import static org.logicng.io.graphical.GraphicalColor.WHITE;

import org.junit.jupiter.api.Test;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNodeStyle;
import org.logicng.io.graphical.GraphicalRepresentation;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PropositionalParser;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.BDDFactory;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * Unit tests for {@link BddGraphicalGenerator}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class BddGraphicalGeneratorTest {

    @Test
    public void testFormulas() throws IOException, ParserException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final List<Variable> ordering = Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C"), f.variable("D"));
        final BDDKernel kernel = new BDDKernel(f, ordering, 1000, 1000);
        testFiles("false", BDDFactory.build(p.parse("$false"), kernel), BddGraphicalGenerator.builder().build());
        testFiles("true", BDDFactory.build(p.parse("$true"), kernel), BddGraphicalGenerator.builder().build());
        testFiles("a", BDDFactory.build(p.parse("A"), kernel), BddGraphicalGenerator.builder().build());
        testFiles("not_a", BDDFactory.build(p.parse("~A"), kernel), BddGraphicalGenerator.builder().build());
        testFiles("impl", BDDFactory.build(p.parse("A => ~C"), kernel), BddGraphicalGenerator.builder().build());
        testFiles("equiv", BDDFactory.build(p.parse("A <=> ~C"), kernel), BddGraphicalGenerator.builder().build());
        testFiles("or", BDDFactory.build(p.parse("A | B | ~C"), kernel), BddGraphicalGenerator.builder().build());
        testFiles("and", BDDFactory.build(p.parse("A & B & ~C"), kernel), BddGraphicalGenerator.builder().build());
        testFiles("not", BDDFactory.build(p.parse("~(A & B & ~C)"), kernel), BddGraphicalGenerator.builder().build());
        testFiles("formula", BDDFactory.build(p.parse("(A => (B|~C)) & (B => C & D) & (D <=> A)"), kernel), BddGraphicalGenerator.builder().build());
    }

    @Test
    public void testFixedStyle() throws ParserException, IOException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final List<Variable> ordering = Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C"), f.variable("D"));
        final BDDKernel kernel = new BDDKernel(f, ordering, 1000, 1000);
        final BDD bdd = BDDFactory.build(p.parse("(A => (B|~C)) & (B => C & D) & (D <=> A)"), kernel);

        final BddGraphicalGenerator translator = BddGraphicalGenerator.builder()
                .falseNodeStyle(new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, PURPLE, WHITE, PURPLE))
                .trueNodeStyle(new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, CYAN, WHITE, CYAN))
                .negativeEdgeStyle(new GraphicalEdgeStyle(GraphicalEdgeStyle.EdgeType.DOTTED, PURPLE))
                .edgeStyle(new GraphicalEdgeStyle(GraphicalEdgeStyle.EdgeType.BOLD, CYAN))
                .nodeStyle(new GraphicalNodeStyle(GraphicalNodeStyle.Shape.CIRCLE, ORANGE, BLACK, ORANGE))
                .backgroundColor(GRAY_LIGHT)
                .alignTerminals(true)
                .build();
        testFiles("formula-fixedStyle", bdd, translator);
    }

    @Test
    public void testDynamic() throws ParserException, IOException {
        final FormulaFactory f = new FormulaFactory();
        final PropositionalParser p = new PropositionalParser(f);
        final List<Variable> ordering = Arrays.asList(f.variable("A"), f.variable("B"), f.variable("C"), f.variable("D"));
        final BDDKernel kernel = new BDDKernel(f, ordering, 1000, 1000);
        final BDD bdd = BDDFactory.build(p.parse("(A => (B|~C)) & (B => C & D) & (D <=> A)"), kernel);

        testFiles("formula-dynamic", bdd, BddGraphicalGenerator.builder().build(), new MyMapperNode(kernel));
    }

    private void testFiles(final String fileName, final BDD bdd, final BddGraphicalGenerator translator) throws IOException {
        testFiles(fileName, bdd, translator, null);
    }

    private void testFiles(final String fileName, final BDD bdd, final BddGraphicalGenerator translator, final NodeStyleMapper<Integer> mapper) throws IOException {
        final GraphicalRepresentation representation = mapper == null ? translator.translate(bdd) : translator.translate(bdd, mapper);
        representation.writeDot("src/test/resources/writers/temp/" + fileName + "_bdd.dot");
        representation.writeMermaid("src/test/resources/writers/temp/" + fileName + "_bdd.txt");
        final File expectedT = new File("src/test/resources/writers/bdd/" + fileName + "_bdd.dot");
        final File tempT = new File("src/test/resources/writers/temp/" + fileName + "_bdd.dot");
        assertThat(contentOf(tempT)).isEqualTo(contentOf(expectedT));
    }

    private static class MyMapperNode extends BddNodeStyleMapper {

        final GraphicalNodeStyle falseStyle = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, RED, RED, WHITE);
        final GraphicalNodeStyle trueStyle = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, GREEN, GREEN, WHITE);
        final GraphicalNodeStyle bStyle = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.CIRCLE, ORANGE, BLACK, ORANGE);
        final GraphicalNodeStyle otherStyle = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.CIRCLE, CYAN, WHITE, CYAN);

        public MyMapperNode(final BDDKernel kernel) {
            super(kernel);
        }

        @Override
        public GraphicalNodeStyle computeStyle(final Integer index) {
            if (isFalse(index)) {
                return this.falseStyle;
            } else if (isTrue(index)) {
                return this.trueStyle;
            } else {
                final Variable variable = variable(index);
                if (variable.name().equals("B")) {
                    return this.bStyle;
                } else {
                    return this.otherStyle;
                }
            }
        }
    }
}
