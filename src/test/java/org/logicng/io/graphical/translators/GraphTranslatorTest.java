package org.logicng.io.graphical.translators;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.contentOf;
import static org.logicng.io.graphical.GraphicalColor.BLACK;
import static org.logicng.io.graphical.GraphicalColor.GREEN;
import static org.logicng.io.graphical.GraphicalColor.ORANGE;
import static org.logicng.io.graphical.GraphicalColor.RED;
import static org.logicng.io.graphical.GraphicalColor.WHITE;

import org.junit.jupiter.api.Test;
import org.logicng.graphs.datastructures.Graph;
import org.logicng.graphs.datastructures.GraphTest;
import org.logicng.io.graphical.GraphicalColor;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNodeStyle;
import org.logicng.io.graphical.GraphicalRepresentation;

import java.io.File;
import java.io.IOException;

/**
 * Unit tests for {@link GraphTranslator}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphTranslatorTest {

    @Test
    public void testSmallDefault() throws IOException {
        final Graph<String> g = new Graph<>();
        g.connect(g.node("A"), g.node("B"));
        g.node("C");
        testFiles("small", g, GraphTranslator.builder().build());
    }

    @Test
    public void testSmallFixedStyle() throws IOException {
        final Graph<String> g = new Graph<>();
        g.connect(g.node("A"), g.node("B"));
        g.node("C");
        final GraphicalTranslatorBuilder<GraphTranslator> builder = GraphTranslator.builder();
        builder.backgroundColor(GraphicalColor.hex("#4f4f4f"));
        builder.nodeStyle(new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, RED, GREEN, ORANGE));
        builder.edgeStyle(new GraphicalEdgeStyle(GraphicalEdgeStyle.LineType.DOTTED, WHITE));
        final GraphTranslator translator = builder
                .build();
        testFiles("small-fixedStyle", g, translator);
    }

    @Test
    public void test30() throws IOException {
        final Graph<Long> g = GraphTest.getLongGraph("30");
        for (long i = 0; i < 30; i++) {
            g.node(i);
        }
        testFiles("30", g, GraphTranslator.builder().build());
    }

    @Test
    public void test30DynamicStyle() throws IOException {
        final Graph<Long> g = GraphTest.getLongGraph("30");
        for (long i = 0; i < 30; i++) {
            g.node(i);
        }
        final GraphicalNodeStyle style1 = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, GREEN, BLACK, GREEN);
        final GraphicalNodeStyle style2 = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.ELLIPSE, ORANGE, BLACK, ORANGE);
        final GraphicalNodeStyle style3 = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.CIRCLE, RED, WHITE, RED);
        final StyleMapper<Long> mapper = (l) -> {
            if (l <= 10) {
                return style1;
            } else if (l <= 20) {
                return style2;
            } else {
                return style3;
            }
        };
        testFiles("30-dynamic", g, GraphTranslator.builder().build(), mapper);
    }

    @Test
    public void test50p1() throws IOException {
        final Graph<Long> g = GraphTest.getLongGraph("50");
        g.node(51L);
        testFiles("50p1", g, GraphTranslator.builder().build());
    }

    private <T> void testFiles(final String fileName, final Graph<T> g, final GraphTranslator translator) throws IOException {
        testFiles(fileName, g, translator, null);
    }

    private <T> void testFiles(final String fileName, final Graph<T> g, final GraphTranslator translator, final StyleMapper<T> mapper) throws IOException {
        if (mapper == null) {
            final GraphicalRepresentation representation = translator.translate(g);
            representation.writeDot("src/test/resources/writers/temp/" + fileName + ".dot");
            representation.writeMermaid("src/test/resources/writers/temp/" + fileName + ".txt");
        } else {
            final GraphicalRepresentation representation = translator.translate(g, mapper);
            representation.writeDot("src/test/resources/writers/temp/" + fileName + ".dot");
            representation.writeMermaid("src/test/resources/writers/temp/" + fileName + ".txt");
        }
        final File expectedDot = new File("src/test/resources/writers/graph/" + fileName + ".dot");
        final File tempDot = new File("src/test/resources/writers/temp/" + fileName + ".dot");
        assertThat(contentOf(tempDot)).isEqualTo(contentOf(expectedDot));

        final File expectedMermaid = new File("src/test/resources/writers/graph/" + fileName + ".txt");
        final File tempMermaid = new File("src/test/resources/writers/temp/" + fileName + ".txt");
        assertThat(contentOf(tempMermaid)).isEqualTo(contentOf(expectedMermaid));
    }
}
