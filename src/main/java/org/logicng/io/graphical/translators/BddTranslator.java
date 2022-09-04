package org.logicng.io.graphical.translators;

import static org.logicng.io.graphical.GraphicalColor.GREEN;
import static org.logicng.io.graphical.GraphicalColor.RED;
import static org.logicng.io.graphical.GraphicalColor.WHITE;

import org.logicng.io.graphical.GraphicalEdge;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNode;
import org.logicng.io.graphical.GraphicalNodeStyle;
import org.logicng.io.graphical.GraphicalRepresentation;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDOperations;

import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;

public class BddTranslator extends GraphicalTranslator {

    private final GraphicalNodeStyle trueNodeStyle;
    private final GraphicalNodeStyle falseNodeStyle;
    private final GraphicalEdgeStyle negativeEdgeStyle;

    BddTranslator(final GraphicalTranslatorBuilder<BddTranslator> builder) {
        super(builder.getBackgroundColor(), builder.isAlginTerminal(), builder.getEdgeStyle(), builder.getNodeStyle());
        final BddTranslatorBuilder bddBuilder = (BddTranslatorBuilder) builder;
        this.negativeEdgeStyle = bddBuilder.negativeEdgeStyle;
        this.trueNodeStyle = bddBuilder.trueNodeStyle;
        this.falseNodeStyle = bddBuilder.falseNodeStyle;
    }

    public static BddTranslatorBuilder builder() {
        return new BddTranslatorBuilder(BddTranslator::new);
    }

    public GraphicalRepresentation translate(final BDD bdd) {
        final StyleMapper<Integer> styleMapper = (index) -> {
            if (index == BDDKernel.BDD_FALSE) {
                return this.falseNodeStyle;
            } else if (index == BDDKernel.BDD_TRUE) {
                return this.trueNodeStyle;
            } else {
                return this.nodeStyle;
            }
        };
        return translate(bdd, styleMapper);
    }

    public GraphicalRepresentation translate(final BDD bdd, final StyleMapper<Integer> styleMapper) {
        final Map<Integer, GraphicalNode> index2Node = new TreeMap<>();

        final GraphicalRepresentation graphicalRepresentation = new GraphicalRepresentation(this.alignTerminals, true, this.backgroundColor);
        if (!bdd.isTautology()) {
            final GraphicalNode falseNode = new GraphicalNode(ID + BDDKernel.BDD_FALSE, "false", true, styleMapper.computeStyle(BDDKernel.BDD_FALSE));
            graphicalRepresentation.addNode(falseNode);
            index2Node.put(BDDKernel.BDD_FALSE, falseNode);
        }
        if (!bdd.isContradiction()) {
            final GraphicalNode trueNode = new GraphicalNode(ID + BDDKernel.BDD_TRUE, "true", true, styleMapper.computeStyle(BDDKernel.BDD_TRUE));
            graphicalRepresentation.addNode(trueNode);
            index2Node.put(BDDKernel.BDD_TRUE, trueNode);
        }
        for (final int[] internalNode : new BDDOperations(bdd.underlyingKernel()).allNodes(bdd.index())) {
            final int index = internalNode[0];
            final String label = bdd.underlyingKernel().getVariableForIndex(internalNode[1]).name();
            final int lowIndex = internalNode[2];
            final int highIndex = internalNode[3];
            final GraphicalNode node = getOrAddNode(index, label, styleMapper, graphicalRepresentation, index2Node);
            final GraphicalNode lowNode = getOrAddNode(lowIndex, label, styleMapper, graphicalRepresentation, index2Node);
            final GraphicalNode highNode = getOrAddNode(highIndex, label, styleMapper, graphicalRepresentation, index2Node);
            graphicalRepresentation.addEdge(new GraphicalEdge(node, lowNode, this.negativeEdgeStyle));
            graphicalRepresentation.addEdge(new GraphicalEdge(node, highNode, this.edgeStyle));
        }
        return graphicalRepresentation;
    }

    private static GraphicalNode getOrAddNode(final int index, final String label, final StyleMapper<Integer> styleMapper,
                                              final GraphicalRepresentation graphicalRepresentation, final Map<Integer, GraphicalNode> index2Node) {
        GraphicalNode node = index2Node.get(index);
        if (node == null) {
            node = new GraphicalNode(ID + index, label, false, styleMapper.computeStyle(index));
            graphicalRepresentation.addNode(node);
            index2Node.put(index, node);
        }
        return node;
    }

    public static class BddTranslatorBuilder extends GraphicalTranslatorBuilder<BddTranslator> {

        private GraphicalNodeStyle trueNodeStyle = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, GREEN, WHITE, GREEN);
        private GraphicalNodeStyle falseNodeStyle = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, RED, WHITE, RED);
        private GraphicalEdgeStyle negativeEdgeStyle = new GraphicalEdgeStyle(GraphicalEdgeStyle.LineType.DOTTED, RED);

        public BddTranslatorBuilder(final Function<GraphicalTranslatorBuilder<BddTranslator>, BddTranslator> constructor) {
            super(constructor);
            this.edgeStyle = new GraphicalEdgeStyle(GraphicalEdgeStyle.LineType.SOLID, GREEN);
        }

        public BddTranslatorBuilder trueNodeStyle(final GraphicalNodeStyle trueNodeStyle) {
            this.trueNodeStyle = trueNodeStyle;
            return this;
        }

        public BddTranslatorBuilder falseNodeStyle(final GraphicalNodeStyle falseNodeStyle) {
            this.falseNodeStyle = falseNodeStyle;
            return this;
        }

        public BddTranslatorBuilder negativeEdgeStyle(final GraphicalEdgeStyle negativeEdgeStyle) {
            this.negativeEdgeStyle = negativeEdgeStyle;
            return this;
        }
    }
}
