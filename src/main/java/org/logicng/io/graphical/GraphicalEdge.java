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

package org.logicng.io.graphical;

import java.util.Objects;

/**
 * A single edge in a graphical representation of a formula, BDD, or graph.  An edge connects two nodes and holds
 * an optional label and an edge style.
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalEdge {
    private final GraphicalNode source;
    private final GraphicalNode destination;
    private final String label;
    private final GraphicalEdgeStyle style;

    /**
     * Constructs a new graphical edge with the given values and without label.
     * @param source      the source node of the edge
     * @param destination the destination node of the edge
     * @param style       the style of this edge
     */
    public GraphicalEdge(final GraphicalNode source, final GraphicalNode destination, final GraphicalEdgeStyle style) {
        this(source, destination, null, style);
    }

    /**
     * Constructs a new graphical edge with the given values.
     * @param source      the source node of the edge
     * @param destination the destination node of the edge
     * @param label       the optional label (can be null)
     * @param style       the style of this edge
     */
    public GraphicalEdge(final GraphicalNode source, final GraphicalNode destination, final String label, final GraphicalEdgeStyle style) {
        this.source = source;
        this.destination = destination;
        this.label = label;
        this.style = style;
    }

    public GraphicalNode getSource() {
        return this.source;
    }

    public GraphicalNode getDestination() {
        return this.destination;
    }

    public String getLabel() {
        return this.label;
    }

    public GraphicalEdgeStyle getStyle() {
        return this.style;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final GraphicalEdge that = (GraphicalEdge) o;
        return Objects.equals(this.source, that.source) &&
                Objects.equals(this.destination, that.destination) &&
                Objects.equals(this.label, that.label) &&
                Objects.equals(this.style, that.style);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.source, this.destination, this.label, this.style);
    }

    @Override
    public String toString() {
        return "GraphicalEdge{" +
                "source=" + this.source +
                ", destination=" + this.destination +
                ", text='" + this.label + '\'' +
                ", style=" + this.style +
                '}';
    }
}
