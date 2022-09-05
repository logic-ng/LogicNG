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
import static org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel.BDD_FALSE;
import static org.logicng.knowledgecompilation.bdds.jbuddy.BDDKernel.BDD_TRUE;

import org.logicng.io.graphical.GraphicalEdge;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNode;
import org.logicng.io.graphical.GraphicalNodeStyle;
import org.logicng.io.graphical.GraphicalRepresentation;
import org.logicng.knowledgecompilation.bdds.BDD;
import org.logicng.knowledgecompilation.bdds.jbuddy.BDDOperations;

import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;

/**
 * The graphical generator for representations of BDDs {@link BDD}.
 * @version 2.4.0
 * @since 2.4.0
 */
public class BddGraphicalGenerator extends GraphicalGenerator<Integer> {

    public static final GraphicalNodeStyle DEFAULT_TRUE_NODE_STYLE = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, GREEN, WHITE, GREEN);
    public static final GraphicalNodeStyle DEFAULT_FALSE_NODE_STYLE = new GraphicalNodeStyle(GraphicalNodeStyle.Shape.RECTANGLE, RED, WHITE, RED);
    public static final GraphicalEdgeStyle DEFAULT_TRUE_EDGE_STYLE = new GraphicalEdgeStyle(GraphicalEdgeStyle.EdgeType.SOLID, GREEN);
    public static final GraphicalEdgeStyle DEFAULT_FALSE_EDGE_STYLE = new GraphicalEdgeStyle(GraphicalEdgeStyle.EdgeType.DOTTED, RED);

    private final GraphicalEdgeStyle negativeEdgeStyle;
    private final EdgeStyleMapper<Integer> negativeEdgeStyleMapper;

    /**
     * Constructs a new generator with the given builder's configuration.
     * @param builder the builder
     */
    BddGraphicalGenerator(final GraphicalGeneratorBuilder<BddGraphicalGenerator, Integer> builder) {
        super(builder.backgroundColor, builder.alginTerminals, builder.defaultEdgeStyle, builder.defaultNodeStyle, builder.nodeStyleMapper,
                builder.labelMapper, builder.edgeMapper);
        final BddTranslatorBuilder bddBuilder = (BddTranslatorBuilder) builder;
        this.negativeEdgeStyle = bddBuilder.negativeEdgeStyle;
        this.negativeEdgeStyleMapper = bddBuilder.negativeEdgeMapper;
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
        final Map<Integer, GraphicalNode> index2Node = new TreeMap<>();

        final GraphicalRepresentation graphicalRepresentation = new GraphicalRepresentation(this.alignTerminals, true, this.backgroundColor);
        if (!bdd.isTautology()) {
            final GraphicalNode falseNode = new GraphicalNode(ID + BDD_FALSE, labelOrDefault(BDD_FALSE, "false"), true, style(BDD_FALSE));
            graphicalRepresentation.addNode(falseNode);
            index2Node.put(BDD_FALSE, falseNode);
        }
        if (!bdd.isContradiction()) {
            final GraphicalNode trueNode = new GraphicalNode(ID + BDD_TRUE, labelOrDefault(BDD_TRUE, "true"), true, style(BDD_TRUE));
            graphicalRepresentation.addNode(trueNode);
            index2Node.put(BDD_TRUE, trueNode);
        }
        for (final int[] internalNode : new BDDOperations(bdd.underlyingKernel()).allNodes(bdd.index())) {
            final int index = internalNode[0];
            final String defaultLabel = bdd.underlyingKernel().getVariableForIndex(internalNode[1]).name();
            addNode(index, labelOrDefault(index, defaultLabel), graphicalRepresentation, index2Node);
        }
        for (final int[] internalNode : new BDDOperations(bdd.underlyingKernel()).allNodes(bdd.index())) {
            final int index = internalNode[0];
            final int lowIndex = internalNode[2];
            final int highIndex = internalNode[3];
            final GraphicalNode node = index2Node.get(index);
            final GraphicalNode lowNode = index2Node.get(lowIndex);
            final GraphicalNode highNode = index2Node.get(highIndex);
            graphicalRepresentation.addEdge(new GraphicalEdge(node, lowNode, negativeEdgeStyle(index, lowIndex)));
            graphicalRepresentation.addEdge(new GraphicalEdge(node, highNode, edgeStyle(index, highIndex)));
        }
        return graphicalRepresentation;
    }

    private void addNode(final int index, final String label, final GraphicalRepresentation graphicalRepresentation,
                         final Map<Integer, GraphicalNode> index2Node) {
        GraphicalNode node = index2Node.get(index);
        if (node == null) {
            node = new GraphicalNode(ID + index, label, false, style(index));
            graphicalRepresentation.addNode(node);
            index2Node.put(index, node);
        }
    }

    private GraphicalEdgeStyle negativeEdgeStyle(final Integer source, final Integer destination) {
        return this.negativeEdgeStyleMapper != null ? this.negativeEdgeStyleMapper.computeStyle(source, destination) : this.negativeEdgeStyle;
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
    public static class BddTranslatorBuilder extends GraphicalGeneratorBuilder<BddGraphicalGenerator, Integer> {

        private GraphicalNodeStyle defaultTrueNodeStyle = DEFAULT_TRUE_NODE_STYLE;
        private GraphicalNodeStyle defaultFalseNodeStyle = DEFAULT_FALSE_NODE_STYLE;
        private GraphicalEdgeStyle negativeEdgeStyle = DEFAULT_FALSE_EDGE_STYLE;
        private EdgeStyleMapper<Integer> negativeEdgeMapper = null;

        /**
         * Constructs a new builder with the given constructor for the graphical generator.
         * @param constructor the constructor for the graphical generator
         */
        BddTranslatorBuilder(final Function<GraphicalGeneratorBuilder<BddGraphicalGenerator, Integer>, BddGraphicalGenerator> constructor) {
            super(constructor);
            this.defaultEdgeStyle = DEFAULT_TRUE_EDGE_STYLE;
            this.nodeStyleMapper = (index) -> {
                if (index == BDD_FALSE) {
                    return this.defaultFalseNodeStyle;
                } else if (index == BDD_TRUE) {
                    return this.defaultTrueNodeStyle;
                } else {
                    return this.defaultNodeStyle;
                }
            };
        }

        /**
         * Sets the default style for the TRUE terminal node of the BDD.  This style will be ignored when a dynamic
         * node style mapper is configured.
         * @param trueNodeStyle the node style
         * @return the current builder
         */
        public BddTranslatorBuilder trueNodeStyle(final GraphicalNodeStyle trueNodeStyle) {
            this.defaultTrueNodeStyle = trueNodeStyle;
            return this;
        }

        /**
         * Sets the default style for the FALSE terminal node of the BDD.  This style will be ignored when a dynamic
         * node style mapper is configured.
         * @param falseNodeStyle the node style
         * @return the current builder
         */
        public BddTranslatorBuilder falseNodeStyle(final GraphicalNodeStyle falseNodeStyle) {
            this.defaultFalseNodeStyle = falseNodeStyle;
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

        /**
         * Sets the negative edge mapper for dynamically computing edge styles for negative edges in the BDD.  If this mapper is configured,
         * the default edge style for negative edges is ignored and each edge is styled by the computed style of
         * {@link EdgeStyleMapper#computeStyle(Object, Object)}.
         * @param negativeEdgeMapper the edge mapper
         * @return the current builder
         */
        public BddTranslatorBuilder negativeEdgeMapper(final EdgeStyleMapper<Integer> negativeEdgeMapper) {
            this.negativeEdgeMapper = negativeEdgeMapper;
            return this;
        }
    }
}
