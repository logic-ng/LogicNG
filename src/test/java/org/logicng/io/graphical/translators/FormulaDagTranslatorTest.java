package org.logicng.io.graphical.translators;

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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.io.graphical.GraphicalColor;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNodeStyle;
import org.logicng.io.graphical.GraphicalRepresentation;
import org.logicng.io.parsers.ParserException;
import org.logicng.io.parsers.PseudoBooleanParser;

import java.io.File;
import java.io.IOException;

/**
 * Unit tests for {@link FormulaDagTranslator}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class FormulaDagTranslatorTest {
    private FormulaFactory f;
    private PseudoBooleanParser p;

    @BeforeEach
    public void init() {
        this.f = new FormulaFactory();
        this.p = new PseudoBooleanParser(this.f);
    }

    @Test
    public void testConstants() throws IOException {
        testFiles("false", this.f.falsum(), FormulaDagTranslator.builder().build());
        testFiles("true", this.f.verum(), FormulaDagTranslator.builder().build());
    }

    @Test
    public void testLiterals() throws IOException {
        testFiles("x", this.f.variable("x"), FormulaDagTranslator.builder().build());
        testFiles("not_x", this.f.literal("x", false), FormulaDagTranslator.builder().build());
    }

    @Test
    public void testFormulas() throws IOException, ParserException {
        final Formula f1 = this.p.parse("(a & b) <=> (~c => (x | z))");
        final Formula f2 = this.p.parse("a & b | b & ~c");
        final Formula f3 = this.p.parse("(a & b) <=> (~c => (a | b))");
        final Formula f4 = this.p.parse("~(a & b) | b & ~c");
        final Formula f5 = this.p.parse("a | ~b | (2*a + 3*~b + 4*c <= 23)");
        testFiles("f1", f1, FormulaDagTranslator.builder().build());
        testFiles("f2", f2, FormulaDagTranslator.builder().build());
        testFiles("f3", f3, FormulaDagTranslator.builder().build());
        testFiles("f4", f4, FormulaDagTranslator.builder().build());
        testFiles("f5", f5, FormulaDagTranslator.builder().build());
    }

    @Test
    public void testDuplicateFormulaParts() throws ParserException, IOException {
        final Formula f6 = this.p.parse("(a & b) | (c & ~(a & b))");
        testFiles("f6", f6, FormulaDagTranslator.builder().build());
        final Formula f7 = this.p.parse("(c & d) | (a & b) | ((c & d) <=> (a & b))");
        testFiles("f7", f7, FormulaDagTranslator.builder().build());
    }

    @Test
    public void testFixedStyle() throws ParserException, IOException {
        final Formula f8 = this.p.parse("(A <=> B & (~A | C | X)) => a + b + c <= 2");
        final FormulaDagTranslator translator = FormulaDagTranslator.builder()
                .backgroundColor("#020202")
                .edgeStyle(new GraphicalEdgeStyle(GraphicalEdgeStyle.LineType.BOLD, CYAN))
                .nodeStyle(new GraphicalNodeStyle(GraphicalNodeStyle.Shape.CIRCLE, BLUE, WHITE, BLUE))
                .alignTerminals(true)
                .build();
        testFiles("f8", f8, translator);
    }

    @Test
    public void testDynamicStyle() throws ParserException, IOException {
        final Formula f9 = this.p.parse("(A <=> B & (~A | C | X)) => a + b + c <= 2 & (~a | d => X & ~B)");

        final GraphicalNodeStyle style1 = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, GRAY_DARK, GRAY_DARK, GRAY_LIGHT);
        final GraphicalNodeStyle style2 = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.CIRCLE, YELLOW, BLACK, YELLOW);
        final GraphicalNodeStyle style3 = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.CIRCLE, TURQUOISE, WHITE, TURQUOISE);
        final GraphicalNodeStyle style4 = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.ELLIPSE, BLACK, BLACK, WHITE);

        final StyleMapper<Formula> mapper = (formula) -> {
            if (formula.type() == FType.PBC) {
                return style1;
            } else if (formula.type() == FType.LITERAL) {
                final Literal lit = (Literal) formula;
                return Character.isLowerCase(lit.name().charAt(0)) ? style2 : style3;
            } else {
                return style4;
            }
        };

        final FormulaDagTranslator translator = FormulaDagTranslator.builder()
                .edgeStyle(new GraphicalEdgeStyle(GraphicalEdgeStyle.LineType.SOLID, PURPLE))
                .build();

        testFiles("f9", f9, translator, mapper);
    }

    @Test
    public void test() {
        System.out.println(GraphicalColor.rgb(0, 0, 0).getHexValue());
    }

    private void testFiles(final String fileName, final Formula formula, final FormulaDagTranslator translator) throws IOException {
        testFiles(fileName, formula, translator, null);
    }

    private void testFiles(final String fileName, final Formula formula, final FormulaDagTranslator translator, final StyleMapper<Formula> mapper) throws IOException {
        if (mapper == null) {
            final GraphicalRepresentation representation = translator.translate(formula);
            representation.writeDot("src/test/resources/writers/temp/" + fileName + ".dot");
            representation.writeMermaid("src/test/resources/writers/temp/" + fileName + ".txt");
        } else {
            final GraphicalRepresentation representation = translator.translate(formula, mapper);
            representation.writeDot("src/test/resources/writers/temp/" + fileName + ".dot");
            representation.writeMermaid("src/test/resources/writers/temp/" + fileName + ".txt");
        }
        final File expectedDot = new File("src/test/resources/writers/formulas-dag/" + fileName + ".dot");
        final File tempDot = new File("src/test/resources/writers/temp/" + fileName + ".dot");
        assertThat(contentOf(tempDot)).isEqualTo(contentOf(expectedDot));

        final File expectedMermaid = new File("src/test/resources/writers/formulas-dag/" + fileName + ".txt");
        final File tempMermaid = new File("src/test/resources/writers/temp/" + fileName + ".txt");
        assertThat(contentOf(tempMermaid)).isEqualTo(contentOf(expectedMermaid));
    }
}
