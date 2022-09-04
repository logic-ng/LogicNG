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

import org.logicng.io.graphical.GraphicalColor;
import org.logicng.io.graphical.GraphicalEdgeStyle;
import org.logicng.io.graphical.GraphicalNodeStyle;

/**
 * An abstract super class for graphical generators.
 * @param <C> the type of the content from which the nodes are generated
 * @version 2.4.0
 * @since 2.4.0
 */
public abstract class GraphicalGenerator<C> {
    protected static final String ID = "id";

    protected final GraphicalColor backgroundColor;
    protected final boolean alignTerminals;
    protected final GraphicalEdgeStyle defaultEdgeStyle;
    protected final GraphicalNodeStyle defaultNodeStyle;
    private final NodeStyleMapper<C> nodeStyleMapper;
    private final LabelMapper<C> labelMapper;
    private final EdgeStyleMapper<C> edgeStyleMapper;

    protected GraphicalGenerator(final GraphicalColor backgroundColor, final boolean alignTerminals, final GraphicalEdgeStyle defaultEdgeStyle,
                                 final GraphicalNodeStyle defaultNodeStyle, final NodeStyleMapper<C> nodeStyleMapper, final LabelMapper<C> labelMapper,
                                 final EdgeStyleMapper<C> edgeStyleMapper) {
        this.backgroundColor = backgroundColor;
        this.alignTerminals = alignTerminals;
        this.defaultEdgeStyle = defaultEdgeStyle;
        this.defaultNodeStyle = defaultNodeStyle;
        this.nodeStyleMapper = nodeStyleMapper;
        this.labelMapper = labelMapper;
        this.edgeStyleMapper = edgeStyleMapper;
    }

    /**
     * Computes the style for a node.  If no style mapper is configured the default style is applied, otherwise, the
     * style mapper is used to dynamically compute the style for the given content.
     * @param content the content to style
     * @return the node style
     */
    protected GraphicalNodeStyle style(final C content) {
        return this.nodeStyleMapper != null ? this.nodeStyleMapper.computeStyle(content) : this.defaultNodeStyle;
    }

    /**
     * Computes the label with the label mapper or returns the default value if no label mapper is configured.
     * @param content      the content for which to compute the label
     * @param defaultLabel the default label
     * @return the label
     */
    protected String labelOrDefault(final C content, final String defaultLabel) {
        return this.labelMapper == null ? defaultLabel : this.labelMapper.computeLabel(content);
    }

    /**
     * Computes the style for an edge.  If no style mapper is configured the default style is applied, otherwise, the
     * style mapper is used to dynamically compute the style for the given edge.
     * @param source      the content of the source node
     * @param destination the content of the destination node
     * @return the node style
     */
    protected GraphicalEdgeStyle edgeStyle(final C source, final C destination) {
        return this.edgeStyleMapper != null ? this.edgeStyleMapper.computeStyle(source, destination) : this.defaultEdgeStyle;
    }
}
