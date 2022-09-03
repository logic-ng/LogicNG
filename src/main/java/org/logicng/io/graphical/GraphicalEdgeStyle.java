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

public class GraphicalEdgeStyle {
    public enum LineType {SOLID, DOTTED, BOLD}

    private final LineType lineType;
    private final GraphicalColor color;

    public GraphicalEdgeStyle() {
        this.lineType = LineType.SOLID;
        this.color = BLACK;
    }

    public GraphicalEdgeStyle(final LineType lineType, final GraphicalColor color) {
        this.lineType = lineType;
        this.color = color;
    }

    public LineType getLineType() {
        return this.lineType;
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
        return this.lineType == that.lineType && Objects.equals(this.color, that.color);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.lineType, this.color);
    }

    @Override
    public String toString() {
        return "GraphicalEdgeStyle{" +
                "lineType=" + this.lineType +
                ", color=" + this.color +
                '}';
    }
}
