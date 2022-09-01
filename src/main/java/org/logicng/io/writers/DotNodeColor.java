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

package org.logicng.io.writers;

/**
 * Color information for a dot file node.  This specifies the text, stroke, and background color
 * of a single node type in a dot output.
 * <p>
 * For color names and specification see <a href="https://graphviz.org/docs/attr-types/color/">here</a>.
 * E.g. you can use "red", "blue", "#22ee33", or "transparent".
 * @version 2.4.0
 * @since 2.4.0
 */
public class DotNodeColor {
    private final String strokeColor;
    private final String textColor;
    private final String backgroundColor;

    /**
     * Constructs a new color configuration with default values: black text and strokes and white background.
     */
    public DotNodeColor() {
        this.strokeColor = "black";
        this.textColor = "black";
        this.backgroundColor = "white";
    }

    /**
     * Constructs a new color configuration.
     * @param strokeColor     the color for the node strokes of the node
     * @param textColor       the color for the text of the node
     * @param backgroundColor the color for the background of the node
     */
    public DotNodeColor(final String strokeColor, final String textColor, final String backgroundColor) {
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
}
