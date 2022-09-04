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

import static org.logicng.io.graphical.GraphicalColor.BLACK;

import java.util.Objects;

/**
 * The style of an edge in a graphical representation of a formula, BDD, or graph.  The style consists of the
 * line type and the color of the edge.
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalEdgeStyle {

    /**
     * The different edge types.
     */
    public enum EdgeType {SOLID, DOTTED, BOLD}

    private final EdgeType type;
    private final GraphicalColor color;

    /**
     * Constructs a new default edge style: a solid black line.
     */
    public GraphicalEdgeStyle() {
        this.type = EdgeType.SOLID;
        this.color = BLACK;
    }

    /**
     * Constructs a new edge style with a given line type and color
     * @param type  the edge type
     * @param color the color
     */
    public GraphicalEdgeStyle(final EdgeType type, final GraphicalColor color) {
        this.type = type;
        this.color = color;
    }

    public EdgeType getType() {
        return this.type;
    }

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
