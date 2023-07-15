// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas;

/**
 * A transformation on a formula.
 * @version 2.0.0
 * @since 1.0
 */
@FunctionalInterface
public interface FormulaTransformation {

    /**
     * Returns the transformed formula.
     * @param formula the input formula
     * @param cache   indicated whether the result (and associated predicates) should be cached in the formula's cache.
     * @return the transformed formula
     */
    Formula apply(Formula formula, boolean cache);
}
