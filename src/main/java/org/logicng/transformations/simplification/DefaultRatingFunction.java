// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.simplification;

import org.logicng.formulas.Formula;
import org.logicng.formulas.printer.DefaultStringRepresentation;

/**
 * A rating function which rates a formula with the length of its representation
 * using the {@link DefaultStringRepresentation}.
 * @version 2.3.0
 * @since 2.0.0
 */
public class DefaultRatingFunction implements RatingFunction<Integer> {

    private final DefaultStringRepresentation formatter = new DefaultStringRepresentation();
    private static final DefaultRatingFunction INSTANCE = new DefaultRatingFunction();

    /**
     * @deprecated In the next version, the standard constructor will be replaced by a private constructor.
     * In order to instantiate an object of this class, use the {@link #get()} method.
     */
    @Deprecated
    public DefaultRatingFunction() {
        // Intentionally left empty
    }

    /**
     * Returns the singleton instance of this function.
     * @return an instance of this function
     */
    public static DefaultRatingFunction get() {
        return INSTANCE;
    }

    @Override
    public Integer apply(final Formula formula, final boolean cache) {
        return this.formatter.toString(formula).length();
    }
}
