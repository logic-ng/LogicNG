// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.configurations;

/**
 * The different types of configurations in LogicNG.
 * @version 2.0.0
 * @since 1.1
 */
public enum ConfigurationType {
    FORMULA_FACTORY,
    CNF,
    MINISAT,
    GLUCOSE,
    MAXSAT,
    MUS,
    CC_ENCODER,
    PB_ENCODER,
    FORMULA_RANDOMIZER,
    ADVANCED_SIMPLIFIER
}
