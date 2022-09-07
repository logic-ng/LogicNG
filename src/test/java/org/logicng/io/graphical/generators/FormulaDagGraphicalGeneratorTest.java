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
import static org.logicng.io.graphical.GraphicalColor.BLUE;
import static org.logicng.io.graphical.GraphicalColor.CYAN;
import static org.logicng.io.graphical.GraphicalColor.GRAY_DARK;
import static org.logicng.io.graphical.GraphicalColor.GRAY_LIGHT;
import static org.logicng.io.graphical.GraphicalColor.PURPLE;
import static org.logicng.io.graphical.GraphicalColor.TURQUOISE;
import static org.logicng.io.graphical.GraphicalColor.WHITE;
import static org.logicng.io.graphical.GraphicalColor.YELLOW;
import static org.logicng.io.graphical.GraphicalEdgeStyle.noStyle;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.graphical.GraphicalDotWriter;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalMermaidWriter;
import org.logicng.io.graphical.GraphicalNodeStyle;
import org.logicng.io.graphical.GraphicalRepresentation;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;

import java.io.File;
import java.io.IOException;

/**
 * Unit tests for {@link FormulaDagGraphicalGenerator}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class FormulaDagGraphicalGeneratorTest {
    private FormulaFactory f;
    private PseudoBooleanParser p;

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
        this.p = new PseudoBooleanParser(this.f);
    }

    @Test
    public void testConstants() throws IOException {
        testFiles("false", this.f.falsum(), FormulaDagGraphicalGenerator.builder().build());
        testFiles("true", this.f.verum(), FormulaDagGraphicalGenerator.builder().build());
    }

    @Test
    public void testLiterals() throws IOException {
        testFiles("x", this.f.variable("x"), FormulaDagGraphicalGenerator.builder().build());
        testFiles("not_x", this.f.literal("x", false), FormulaDagGraphicalGenerator.builder().build());
    }

    @Test
    public void testFormulas() throws IOException, ParserException {
        final Formula f1 = this.p.parse("(a & b) <=> (~c => (x | z))");
        final Formula f2 = this.p.parse("a & b | b & ~c");
        final Formula f3 = this.p.parse("(a & b) <=> (~c => (a | b))");
        final Formula f4 = this.p.parse("~(a & b) | b & ~c");
        final Formula f5 = this.p.parse("a | ~b | (2*a + 3*~b + 4*c <= 23)");
        testFiles("f1", f1, FormulaDagGraphicalGenerator.builder().build());
        testFiles("f2", f2, FormulaDagGraphicalGenerator.builder().build());
        testFiles("f3", f3, FormulaDagGraphicalGenerator.builder().build());
        testFiles("f4", f4, FormulaDagGraphicalGenerator.builder().build());
        testFiles("f5", f5, FormulaDagGraphicalGenerator.builder().build());
    }

    @Test
    public void testDuplicateFormulaParts() throws ParserException, IOException {
        final Formula f6 = this.p.parse("(a & b) | (c & ~(a & b))");
        testFiles("f6", f6, FormulaDagGraphicalGenerator.builder().build());
        final Formula f7 = this.p.parse("(c & d) | (a & b) | ((c & d) <=> (a & b))");
        testFiles("f7", f7, FormulaDagGraphicalGenerator.builder().build());
    }

    @Test
    public void testFixedStyle() throws ParserException, IOException {
        final Formula f8 = this.p.parse("(A <=> B & (~A | C | X)) => a + b + c <= 2");
        final FormulaDagGraphicalGenerator generator = FormulaDagGraphicalGenerator.builder()
                .backgroundColor("#020202")
                .defaultEdgeStyle(GraphicalEdgeStyle.bold(CYAN))
                .defaultNodeStyle(GraphicalNodeStyle.circle(BLUE, WHITE, BLUE))
                .alignTerminals(true)
                .build();
        testFiles("f8", f8, generator);
    }

    @Test
    public void testDynamicStyle() throws ParserException, IOException {
        final Formula f9 = this.p.parse("(A <=> B & (~A | C | X)) => a + b + c <= 2 & (~a | d => X & ~B)");

        final GraphicalNodeStyle style1 = GraphicalNodeStyle.rectangle(GRAY_DARK, GRAY_DARK, GRAY_LIGHT);
        final GraphicalNodeStyle style2 = GraphicalNodeStyle.circle(YELLOW, BLACK, YELLOW);
        final GraphicalNodeStyle style3 = GraphicalNodeStyle.circle(TURQUOISE, WHITE, TURQUOISE);
        final GraphicalNodeStyle style4 = GraphicalNodeStyle.ellipse(BLACK, BLACK, WHITE);

        final NodeStyleMapper<Formula> mapper = (formula) -> {
            if (formula.type() == FType.PBC) {
                return style1;
            } else if (formula.type() == FType.LITERAL) {
                final Literal lit = (Literal) formula;
                return Character.isLowerCase(lit.name().charAt(0)) ? style2 : style3;
            } else {
                return style4;
            }
        };

        final FormulaDagGraphicalGenerator generator = FormulaDagGraphicalGenerator.builder()
                .defaultEdgeStyle(GraphicalEdgeStyle.solid(PURPLE))
                .nodeStyleMapper(mapper)
                .labelMapper((f) -> "Formula Type: " + f.type())
                .build();

        testFiles("f9", f9, generator);
    }

    @Test
    public void testEdgeMapper() throws ParserException, IOException {
        final Formula f10 = this.p.parse("(A <=> B & (~A | C | X)) => a + b + c <= 2 & (~a | d => X & ~B)");

        final GraphicalEdgeStyle style1 = GraphicalEdgeStyle.dotted(GRAY_DARK);

        final EdgeStyleMapper<Formula> edgeMapper = (source, dest) -> {
            if (source.type() == FType.PBC) {
                return style1;
            } else {
                return noStyle();
            }
        };

        final FormulaDagGraphicalGenerator generator = FormulaDagGraphicalGenerator.builder()
                .defaultEdgeStyle(GraphicalEdgeStyle.solid(PURPLE))
                .edgeMapper(edgeMapper)
                .build();

        testFiles("f10", f10, generator);
    }

    private void testFiles(final String fileName, final Formula formula, final FormulaDagGraphicalGenerator generator) throws IOException {
        final GraphicalRepresentation representation = generator.translate(formula);
        representation.write("src/test/resources/writers/temp/" + fileName + ".dot", GraphicalDotWriter.get());
        representation.write("src/test/resources/writers/temp/" + fileName + ".txt", GraphicalMermaidWriter.get());

        final File expectedDot = new File("src/test/resources/writers/formulas-dag/" + fileName + ".dot");
        final File tempDot = new File("src/test/resources/writers/temp/" + fileName + ".dot");
        assertThat(contentOf(tempDot)).isEqualTo(contentOf(expectedDot));

        final File expectedMermaid = new File("src/test/resources/writers/formulas-dag/" + fileName + ".txt");
        final File tempMermaid = new File("src/test/resources/writers/temp/" + fileName + ".txt");
        assertThat(contentOf(tempMermaid)).isEqualTo(contentOf(expectedMermaid));
    }
}
