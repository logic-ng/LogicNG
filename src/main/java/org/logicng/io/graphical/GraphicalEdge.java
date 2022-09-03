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

public class GraphicalEdge {
    private final GraphicalNode source;
    private final GraphicalNode destination;
    private final String label;
    private final GraphicalEdgeStyle style;

    public GraphicalEdge(final GraphicalNode source, final GraphicalNode destination, final GraphicalEdgeStyle edgeStyle) {
        this(source, destination, null, edgeStyle);
    }

    public GraphicalEdge(final GraphicalNode source, final GraphicalNode destination, final String label) {
        this(source, destination, label, new GraphicalEdgeStyle());
    }

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
