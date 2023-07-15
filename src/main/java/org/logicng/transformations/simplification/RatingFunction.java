// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.transformations.simplification;

import org.logicng.formulas.FormulaFunction;

/**
 * A rating function for a formula.
 * @param <N> the number type to which the formula is rated
 * @version 2.0.0
 * @since 2.0.0
 */
@FunctionalInterface
public interface RatingFunction<N extends Number> extends FormulaFunction<N> {
}
