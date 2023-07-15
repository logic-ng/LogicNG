// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.io.graphical;

import java.util.Objects;

/**
 * A color for graphical representations of formulas, BDDs, and graphs.
 * <p>
 * A color can be generated from RGB or hex values.
 * @version 2.4.0
 * @since 2.4.0
 */
public class GraphicalColor {

    public static final GraphicalColor BLACK = new GraphicalColor("#000000");
    public static final GraphicalColor WHITE = new GraphicalColor("#ffffff");
    public static final GraphicalColor GRAY_DARK = new GraphicalColor("#777777");
    public static final GraphicalColor GRAY_LIGHT = new GraphicalColor("#e4e4e4");
    public static final GraphicalColor RED = new GraphicalColor("#ea2027");
    public static final GraphicalColor GREEN = new GraphicalColor("#009432");
    public static final GraphicalColor BLUE = new GraphicalColor("#004f93");
    public static final GraphicalColor YELLOW = new GraphicalColor("#ffc612");
    public static final GraphicalColor ORANGE = new GraphicalColor("#f79f1f");
    public static final GraphicalColor CYAN = new GraphicalColor("#1289a7");
    public static final GraphicalColor PURPLE = new GraphicalColor("#5758bb");
    public static final GraphicalColor TURQUOISE = new GraphicalColor("#006266");

    private final String hexValue;

    /**
     * Private constructor.  Use factory methods to construct an instance.
     * @param hexValue the hex value
     */
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
     * Constructs a new color from an RGB value.  Each value must be within range 0 (inclusive) and 255 (inclusive).
     * @param red   the red value
     * @param green the green value
     * @param blue  the blue value
     * @return the color object
     */
    public static GraphicalColor rgb(final int red, final int green, final int blue) {
        if (!isValidRgbValue(red) || !isValidRgbValue(green) || !isValidRgbValue(blue)) {
            throw new IllegalArgumentException("Invalid RGB value (must be within range 0 (inclusive) and 255 (inclusive)).");
        }
        final String hex = String.format("#%2s%2s%2s", Integer.toHexString(red), Integer.toHexString(green), Integer.toHexString(blue)).replace(" ", "0");
        return new GraphicalColor(hex);
    }

    /**
     * Returns the hex value of this color.
     * @return the hex value of this color
     */
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

    private static boolean isValidRgbValue(final int rgbValue) {
        return 0 <= rgbValue && rgbValue <= 255;
    }
}
