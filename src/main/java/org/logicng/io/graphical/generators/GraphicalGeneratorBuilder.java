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

import static org.logicng.io.graphical.GraphicalColor.WHITE;

import org.logicng.io.graphical.GraphicalColor;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNodeStyle;

import java.util.function.Function;

/**
 * A builder for graphical generators with some common options.
 * @param <T> the type of the generator which is built at the end
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalGeneratorBuilder<T extends GraphicalGenerator> {

    protected GraphicalColor backgroundColor = WHITE;
    protected boolean alginTerminal;
    protected GraphicalEdgeStyle edgeStyle = new GraphicalEdgeStyle();
    protected GraphicalNodeStyle nodeStyle = new GraphicalNodeStyle();
    protected final Function<GraphicalGeneratorBuilder<T>, T> constructor;

    /**
     * Constructs a new builder with the given constructor for the graphical generator.
     * @param constructor the constructor for the graphical generator
     */
    GraphicalGeneratorBuilder(final Function<GraphicalGeneratorBuilder<T>, T> constructor) {
        this.constructor = constructor;
    }

    /**
     * Sets the background color for the graph.
     * @param color the color
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T> backgroundColor(final GraphicalColor color) {
        this.backgroundColor = color;
        return this;
    }

    /**
     * Sets the background color for the graph as hex color value.
     * @param hexColor the hex color value in the format "#beef41"
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T> backgroundColor(final String hexColor) {
        this.backgroundColor = GraphicalColor.hex(hexColor);
        return this;
    }

    /**
     * Sets the background color for the graph as RGB value
     * @param red   the red value
     * @param green the green value
     * @param blue  the blue value
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T> backgroundColor(final int red, final int green, final int blue) {
        this.backgroundColor = GraphicalColor.rgb(red, green, blue);
        return this;
    }

    /**
     * Sets whether all terminal nodes of the graph should be layouted on the same level.
     * <p>
     * This flag is only applied to BDD and formula DAG and AST graphs. It can only be layouted
     * by DOT, not by Mermaid.js.
     * @param alignTerminals whether the terminal nodes should be on the same level
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T> alignTerminals(final boolean alignTerminals) {
        this.alginTerminal = alignTerminals;
        return this;
    }

    /**
     * The default edge style for all edges in the graph.  In a BDD graph this is the style
     * of the positive (high) edges.
     * @param edgeStyle the edge style
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T> edgeStyle(final GraphicalEdgeStyle edgeStyle) {
        this.edgeStyle = edgeStyle;
        return this;
    }

    /**
     * The default node style for all nodes in the graph.  If no dynamic node styling (c.f. {@link NodeStyleMapper}) is used,
     * all nodes have this style
     * @param nodeStyle the node style
     * @return the current builder
     */
    public GraphicalGeneratorBuilder<T> nodeStyle(final GraphicalNodeStyle nodeStyle) {
        this.nodeStyle = nodeStyle;
        return this;
    }

    /**
     * Returns the graphical generator with this builder's configuration.
     * @return the graphical generator
     */
    public T build() {
        return this.constructor.apply(this);
    }

    public GraphicalColor getBackgroundColor() {
        return this.backgroundColor;
    }

    public boolean isAlginTerminal() {
        return this.alginTerminal;
    }

    public GraphicalEdgeStyle getEdgeStyle() {
        return this.edgeStyle;
    }

    public GraphicalNodeStyle getNodeStyle() {
        return this.nodeStyle;
    }
}
