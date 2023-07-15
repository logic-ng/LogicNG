// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

/**
 * A function on a formula.
 * @param <T> the result type of the function
 * @version 2.0.0
 * @since 1.0
 */
@FunctionalInterface
public interface FormulaFunction<T> {

    /**
     * Applies this function to a given formula.
     * @param formula the input formula
     * @param cache   indicates whether the result should be cached in this formula's cache
     * @return the result of the application
     */
    T apply(Formula formula, boolean cache);
}
