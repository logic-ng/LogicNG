// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.graphical.generators;

import org.logicng.io.graphical.GraphicalColor;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNodeStyle;

import java.util.function.Function;

/**
 * A builder for graphical generators with some common options.
 * @param <T> the type of the generator which is built at the end
 * @param <C> the type of content from which the nodes are generated
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalGeneratorBuilder<T extends GraphicalGenerator<C>, C> {

    protected GraphicalColor backgroundColor = null;
    protected boolean alignTerminals;
    protected GraphicalEdgeStyle defaultEdgeStyle = GraphicalEdgeStyle.noStyle();
    protected GraphicalNodeStyle defaultNodeStyle = GraphicalNodeStyle.noStyle();
    protected NodeStyleMapper<C> nodeStyleMapper = null;
    protected LabelMapper<C> labelMapper = null;
    protected EdgeStyleMapper<C> edgeMapper = null;
    protected final Function<GraphicalGeneratorBuilder<T, C>, T> constructor;

    /**
     * Constructs a new builder with the given constructor for the graphical
     * generator.
     * @param constructor the constructor for the graphical generator
     */
    GraphicalGeneratorBuilder(final Function<GraphicalGeneratorBuilder<T, C>, T> constructor) {
        this.constructor = constructor;
    }

    /**
     * Sets the background color for the graph.
     * @param color the color
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T, C> backgroundColor(final GraphicalColor color) {
        this.backgroundColor = color;
        return this;
    }

    /**
     * Sets the background color for the graph as hex color value.
     * @param hexColor the hex color value in the format "#aabbcc"
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T, C> backgroundColor(final String hexColor) {
        this.backgroundColor = GraphicalColor.hex(hexColor);
        return this;
    }

    /**
     * Sets the background color for the graph as RGB value.
     * @param red   the red value
     * @param green the green value
     * @param blue  the blue value
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T, C> backgroundColor(final int red, final int green, final int blue) {
        this.backgroundColor = GraphicalColor.rgb(red, green, blue);
        return this;
    }

    /**
     * Sets whether all terminal nodes of the graph should be aligned on the
     * same level.
     * <p>
     * This flag is only applied to BDD and formula DAG and AST graphs. It can
     * only be applied by DOT, not by Mermaid.js.
     * @param alignTerminals whether the terminal nodes should be on the same
     *                       level
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T, C> alignTerminals(final boolean alignTerminals) {
        this.alignTerminals = alignTerminals;
        return this;
    }

    /**
     * Sets the default edge style for all edges in the graph. In a BDD graph
     * this is the style of the positive (high) edges. If no dynamic edge
     * styling is configured via {@link #edgeMapper(EdgeStyleMapper)}, this
     * style will be applied to all edges.
     * @param edgeStyle the edge style
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T, C> defaultEdgeStyle(final GraphicalEdgeStyle edgeStyle) {
        this.defaultEdgeStyle = edgeStyle;
        return this;
    }

    /**
     * Sets the default node style for all nodes in the graph. If no dynamic
     * node styling is configured via
     * {@link #nodeStyleMapper(NodeStyleMapper)})}, this style will be applied
     * to all nodes.
     * @param nodeStyle the node style
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T, C> defaultNodeStyle(final GraphicalNodeStyle nodeStyle) {
        this.defaultNodeStyle = nodeStyle;
        return this;
    }

    /**
     * Sets the node style mapper for dynamically styling nodes in the graph. If
     * this mapper is configured, the default node style is ignored and each
     * node is styled by the computed style of
     * {@link NodeStyleMapper#computeStyle(Object)}.
     * @param nodeStyleMapper the node style mapper
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T, C> nodeStyleMapper(final NodeStyleMapper<C> nodeStyleMapper) {
        this.nodeStyleMapper = nodeStyleMapper;
        return this;
    }

    /**
     * Sets the label mapper for dynamically computing labels for nodes in the
     * graph.
     * @param labelMapper the label mapper
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T, C> labelMapper(final LabelMapper<C> labelMapper) {
        this.labelMapper = labelMapper;
        return this;
    }

    /**
     * Sets the edge mapper for dynamically styling edges in the graph. If this
     * mapper is configured, the default edge style is ignored and each edge is
     * styled by the computed style of
     * {@link EdgeStyleMapper#computeStyle(Object, Object)}. For BDDs this
     * mapper is only used for positive (high) edges.
     * @param edgeMapper the edge mapper
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T, C> edgeMapper(final EdgeStyleMapper<C> edgeMapper) {
        this.edgeMapper = edgeMapper;
        return this;
    }

    /**
     * Returns the graphical generator with this builder's configuration.
     * @return the graphical generator
     */
    public T build() {
        return this.constructor.apply(this);
    }
}
