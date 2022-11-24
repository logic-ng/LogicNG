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
 * The style of an edge in a graphical representation of a formula, BDD, or graph.  The style consists of the
 * line type and the color of the edge.
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalEdgeStyle {

    /**
     * A special edge style which does not set any style, so the framework's (dot/Mermaid.js)
     * default theming is applied.
     */
    private static final GraphicalEdgeStyle NO_EDGE_STYLE = new GraphicalEdgeStyle(null, null);

    /**
     * The different edge types.
     */
    public enum EdgeType {SOLID, DOTTED, BOLD}

    private final EdgeType type;
    private final GraphicalColor color;

    /**
     * Private constructor.  Use factory methods to construct an instance.
     * @param type  the edge type
     * @param color the color
     */
    private GraphicalEdgeStyle(final EdgeType type, final GraphicalColor color) {
        this.type = type;
        this.color = color;
    }

    /**
     * Returns a special edge style which does not set any style, so the framework's (dot/Mermaid.js)
     * default theming is applied.
     * @return special edge style which does not set any style
     */
    public static GraphicalEdgeStyle noStyle() {
        return NO_EDGE_STYLE;
    }

    /**
     * Constructs a new edge style with a given edge type and color.
     * @param type  the edge type
     * @param color the color
     * @return the new edge style
     */
    public static GraphicalEdgeStyle style(final EdgeType type, final GraphicalColor color) {
        if (type == null && color == null) {
            return NO_EDGE_STYLE;
        } else {
            return new GraphicalEdgeStyle(type, color);
        }
    }

    /**
     * Constructs a new solid edge style with a color.
     * @param color the color
     * @return the new edge style
     */
    public static GraphicalEdgeStyle solid(final GraphicalColor color) {
        return new GraphicalEdgeStyle(EdgeType.SOLID, color);
    }

    /**
     * Constructs a new dotted edge style with a color.
     * @param color the color
     * @return the new edge style
     */
    public static GraphicalEdgeStyle dotted(final GraphicalColor color) {
        return new GraphicalEdgeStyle(EdgeType.DOTTED, color);
    }

    /**
     * Constructs a new bold edge style with a color.
     * @param color the color
     * @return the new edge style
     */
    public static GraphicalEdgeStyle bold(final GraphicalColor color) {
        return new GraphicalEdgeStyle(EdgeType.BOLD, color);
    }

    /**
     * Returns whether this style has any value set.  If not it is equivalent to {@link GraphicalEdgeStyle#NO_EDGE_STYLE}.
     * @return whether this style has any value set
     */
    public boolean hasStyle() {
        return !this.equals(NO_EDGE_STYLE);
    }

    /**
     * Returns the type of this edge style.
     * @return the type of this edge style
     */
    public EdgeType getType() {
        return this.type;
    }

    /**
     * Returns the color of this edge style.
     * @return the color of this edge style
     */
    public GraphicalColor getColor() {
        return this.color;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final GraphicalEdgeStyle that = (GraphicalEdgeStyle) o;
        return this.type == that.type && Objects.equals(this.color, that.color);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.type, this.color);
    }

    @Override
    public String toString() {
        return "GraphicalEdgeStyle{" +
                "edgeType=" + this.type +
                ", color=" + this.color +
                '}';
    }
}
