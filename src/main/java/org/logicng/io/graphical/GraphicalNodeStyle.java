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
import static org.logicng.io.graphical.GraphicalColor.WHITE;

import java.util.Objects;

/**
 * Style information for a graphical node.  This specifies the shape and text, stroke, and background color
 * of a single node type in a graphical output.
 * <p>
 * For color names use standard hexadecimal notation with a leading hash, e.g. "#22ee33".
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalNodeStyle {

    public enum Shape {RECTANGLE, ELLIPSE, CIRCLE}

    private final Shape shape;
    private final GraphicalColor strokeColor;
    private final GraphicalColor textColor;
    private final GraphicalColor backgroundColor;

    /**
     * Constructs a new color configuration with default values: black text and strokes and white background.
     */
    public GraphicalNodeStyle() {
        this.shape = Shape.ELLIPSE;
        this.strokeColor = BLACK;
        this.textColor = BLACK;
        this.backgroundColor = WHITE;
    }

    /**
     * Constructs a new node style definition.
     * @param shape           the shape of the node
     * @param strokeColor     the color for the node strokes of the node
     * @param textColor       the color for the text of the node
     * @param backgroundColor the color for the background of the node
     */
    public GraphicalNodeStyle(final Shape shape, final GraphicalColor strokeColor, final GraphicalColor textColor, final GraphicalColor backgroundColor) {
        this.shape = shape;
        this.strokeColor = strokeColor;
        this.textColor = textColor;
        this.backgroundColor = backgroundColor;
    }

    /**
     * Returns a string containing the colors specified in the dot graph format. The string can be used within a dot file for a node.
     * @return the dot string with the colors set
     */
    public String toDotString() {
        return String.format("style=filled, color=\"%s\", fontcolor=\"%s\", fillcolor=\"%s\"", this.strokeColor, this.textColor, this.backgroundColor);
    }

    public Shape getShape() {
        return this.shape;
    }

    public GraphicalColor getStrokeColor() {
        return this.strokeColor;
    }

    public GraphicalColor getTextColor() {
        return this.textColor;
    }

    public GraphicalColor getBackgroundColor() {
        return this.backgroundColor;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final GraphicalNodeStyle that = (GraphicalNodeStyle) o;
        return this.shape == that.shape && Objects.equals(this.strokeColor, that.strokeColor) && Objects.equals(this.textColor, that.textColor) && Objects.equals(this.backgroundColor, that.backgroundColor);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.shape, this.strokeColor, this.textColor, this.backgroundColor);
    }

    @Override
    public String toString() {
        return "GraphicalNodeStyle{" +
                "shape=" + this.shape +
                ", strokeColor='" + this.strokeColor + '\'' +
                ", textColor='" + this.textColor + '\'' +
                ", backgroundColor='" + this.backgroundColor + '\'' +
                '}';
    }
}
