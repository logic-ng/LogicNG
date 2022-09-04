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

/**
 * The graphical generator for representations of BDDs {@link BDD}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class BddGraphicalGenerator extends GraphicalGenerator {

    private final GraphicalNodeStyle trueNodeStyle;
    private final GraphicalNodeStyle falseNodeStyle;
    private final GraphicalEdgeStyle negativeEdgeStyle;

    /**
     * Constructs a new generator with the given builder's configuration.
     * @param builder the builder
     */
    BddGraphicalGenerator(final GraphicalGeneratorBuilder<BddGraphicalGenerator> builder) {
        super(builder.getBackgroundColor(), builder.isAlginTerminal(), builder.getEdgeStyle(), builder.getNodeStyle());
        final BddTranslatorBuilder bddBuilder = (BddTranslatorBuilder) builder;
        this.negativeEdgeStyle = bddBuilder.negativeEdgeStyle;
        this.trueNodeStyle = bddBuilder.trueNodeStyle;
        this.falseNodeStyle = bddBuilder.falseNodeStyle;
    }

    /**
     * Returns the builder for this generator.
     * @return the builder
     */
    public static BddTranslatorBuilder builder() {
        return new BddTranslatorBuilder(BddGraphicalGenerator::new);
    }

    /**
     * Translates a given BDD in its graphical representation.
     * @param bdd the BDD
     * @return the graphical representation
     */
    public GraphicalRepresentation translate(final BDD bdd) {
        final NodeStyleMapper<Integer> nodeStyleMapper = (index) -> {
            if (index == BDDKernel.BDD_FALSE) {
                return this.falseNodeStyle;
            } else if (index == BDDKernel.BDD_TRUE) {
                return this.trueNodeStyle;
            } else {
                return this.nodeStyle;
            }
        };
        return translate(bdd, nodeStyleMapper);
    }

    /**
     * Translates a given BDD in its graphical representation.
     * @param bdd             the BDD
     * @param nodeStyleMapper the node style mapper for dynamically styling nodes
     * @return the graphical representation
     */
    public GraphicalRepresentation translate(final BDD bdd, final NodeStyleMapper<Integer> nodeStyleMapper) {
        final Map<Integer, GraphicalNode> index2Node = new TreeMap<>();

        final GraphicalRepresentation graphicalRepresentation = new GraphicalRepresentation(this.alignTerminals, true, this.backgroundColor);
        if (!bdd.isTautology()) {
            final GraphicalNode falseNode = new GraphicalNode(ID + BDDKernel.BDD_FALSE, "false", true, nodeStyleMapper.computeStyle(BDDKernel.BDD_FALSE));
            graphicalRepresentation.addNode(falseNode);
            index2Node.put(BDDKernel.BDD_FALSE, falseNode);
        }
        if (!bdd.isContradiction()) {
            final GraphicalNode trueNode = new GraphicalNode(ID + BDDKernel.BDD_TRUE, "true", true, nodeStyleMapper.computeStyle(BDDKernel.BDD_TRUE));
            graphicalRepresentation.addNode(trueNode);
            index2Node.put(BDDKernel.BDD_TRUE, trueNode);
        }
        for (final int[] internalNode : new BDDOperations(bdd.underlyingKernel()).allNodes(bdd.index())) {
            final int index = internalNode[0];
            final String label = bdd.underlyingKernel().getVariableForIndex(internalNode[1]).name();
            final int lowIndex = internalNode[2];
            final int highIndex = internalNode[3];
            final GraphicalNode node = getOrAddNode(index, label, nodeStyleMapper, graphicalRepresentation, index2Node);
            final GraphicalNode lowNode = getOrAddNode(lowIndex, label, nodeStyleMapper, graphicalRepresentation, index2Node);
            final GraphicalNode highNode = getOrAddNode(highIndex, label, nodeStyleMapper, graphicalRepresentation, index2Node);
            graphicalRepresentation.addEdge(new GraphicalEdge(node, lowNode, this.negativeEdgeStyle));
            graphicalRepresentation.addEdge(new GraphicalEdge(node, highNode, this.edgeStyle));
        }
        return graphicalRepresentation;
    }

    private static GraphicalNode getOrAddNode(final int index, final String label, final NodeStyleMapper<Integer> nodeStyleMapper,
                                              final GraphicalRepresentation graphicalRepresentation, final Map<Integer, GraphicalNode> index2Node) {
        GraphicalNode node = index2Node.get(index);
        if (node == null) {
            node = new GraphicalNode(ID + index, label, false, nodeStyleMapper.computeStyle(index));
            graphicalRepresentation.addNode(node);
            index2Node.put(index, node);
        }
        return node;
    }

    /**
     * An extension of the builder for graphical generators to include BDD specific values.
     * <p>
     * Since only the additional methods of this builder can return an object of the type {@link BddTranslatorBuilder},
     * you have to configure all BDD-specific values of this builder _before_ you configure the values
     * of the super builder {@link GraphicalGeneratorBuilder}.
     * @version 2.4.0
     * @since 2.4.0
     */
    public static class BddTranslatorBuilder extends GraphicalGeneratorBuilder<BddGraphicalGenerator> {

        private GraphicalNodeStyle trueNodeStyle = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, GREEN, WHITE, GREEN);
        private GraphicalNodeStyle falseNodeStyle = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, RED, WHITE, RED);
        private GraphicalEdgeStyle negativeEdgeStyle = new GraphicalEdgeStyle(GraphicalEdgeStyle.EdgeType.DOTTED, RED);

        /**
         * Constructs a new builder with the given constructor for the graphical generator.
         * @param constructor the constructor for the graphical generator
         */
        BddTranslatorBuilder(final Function<GraphicalGeneratorBuilder<BddGraphicalGenerator>, BddGraphicalGenerator> constructor) {
            super(constructor);
            this.edgeStyle = new GraphicalEdgeStyle(GraphicalEdgeStyle.EdgeType.SOLID, GREEN);
        }

        /**
         * Sets the default style for the TRUE terminal node of the BDD.
         * @param trueNodeStyle the node style
         * @return the current builder
         */
        public BddTranslatorBuilder trueNodeStyle(final GraphicalNodeStyle trueNodeStyle) {
            this.trueNodeStyle = trueNodeStyle;
            return this;
        }

        /**
         * Sets the default style for the FALSE terminal node of the BDD.
         * @param falseNodeStyle the node style
         * @return the current builder
         */
        public BddTranslatorBuilder falseNodeStyle(final GraphicalNodeStyle falseNodeStyle) {
            this.falseNodeStyle = falseNodeStyle;
            return this;
        }

        /**
         * Sets the default edge style for negative (low) edges in the BDD.
         * @param negativeEdgeStyle the edge style
         * @return the current builder
         */
        public BddTranslatorBuilder negativeEdgeStyle(final GraphicalEdgeStyle negativeEdgeStyle) {
            this.negativeEdgeStyle = negativeEdgeStyle;
            return this;
        }
    }
}
