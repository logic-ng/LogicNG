// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.propositions;

import org.logicng.formulas.Formula;

/**
 * An interface for a proposition in LogicNG.  A proposition is a formula with an additional information
 * like a textual description or a user-provided object.
 * @version 2.0.0
 * @since 1.0
 */
public abstract class Proposition {

    /**
     * Returns the formula of this proposition.
     * @return the formula of this proposition
     */
    public abstract Formula formula();
}
