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
 * A color for graphical representations of formulas, BDDs, and graphs.
 * <p>
 * A color can be generated from RGB or Hex values.
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalColor {

    public static GraphicalColor BLACK = new GraphicalColor("#000000");
    public static GraphicalColor WHITE = new GraphicalColor("#ffffff");
    public static GraphicalColor GRAY_DARK = new GraphicalColor("#777777");
    public static GraphicalColor GRAY_LIGHT = new GraphicalColor("#e4e4e4");
    public static GraphicalColor RED = new GraphicalColor("#ea2027");
    public static GraphicalColor GREEN = new GraphicalColor("#009432");
    public static GraphicalColor BLUE = new GraphicalColor("#004f93");
    public static GraphicalColor YELLOW = new GraphicalColor("#ffc612");
    public static GraphicalColor ORANGE = new GraphicalColor("#f79f1f");
    public static GraphicalColor CYAN = new GraphicalColor("#1289a7");
    public static GraphicalColor PURPLE = new GraphicalColor("#5758bb");
    public static GraphicalColor TURQUOISE = new GraphicalColor("#006266");

    private final String hexValue;

    private GraphicalColor(final String hexValue) {
        this.hexValue = hexValue;
    }

    /**
     * Constructs a new color from a hex value.  The string must be of the form "#aabbcc".  E.g. the default
     * red has the representation "#ea2027".
     * @param hexValue the hex value
     * @return the color object
     */
    public static GraphicalColor hex(final String hexValue) {
        return new GraphicalColor(hexValue);
    }

    /**
     * Constructs a new color from an RGB value.  Each value must be between 0 and 255.
     * @param red   the red value
     * @param green the green value
     * @param blue  the blue value
     * @return the color object
     */
    public static GraphicalColor rgb(final int red, final int green, final int blue) {
        if (red < 0 || red > 255 || green < 0 || green > 255 || blue < 0 || blue > 255) {
            throw new IllegalArgumentException("Invalid RGB value (must be between 0 and 255)");
        }
        final String hex = String.format("#%2s%2s%2s", Integer.toHexString(red), Integer.toHexString(green), Integer.toHexString(blue)).replace(" ", "0");
        return new GraphicalColor(hex);
    }

    public String getHexValue() {
        return this.hexValue;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final GraphicalColor that = (GraphicalColor) o;
        return Objects.equals(this.hexValue, that.hexValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.hexValue);
    }

    @Override
    public String toString() {
        return "GraphicalColor{" +
                "hexValue='" + this.hexValue + '\'' +
                '}';
    }
}
