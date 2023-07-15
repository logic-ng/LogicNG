// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.knowledgecompilation.bdds.jbuddy;

/**
 * Enumeration of BDD reordering algorithms.
 * @version 2.0.0
 * @since 2.0.0
 */
public enum BDDReorderingMethod {
    BDD_REORDER_NONE,
    BDD_REORDER_WIN2,
    BDD_REORDER_WIN2ITE,
    BDD_REORDER_SIFT,
    BDD_REORDER_SIFTITE,
    BDD_REORDER_WIN3,
    BDD_REORDER_WIN3ITE,
    BDD_REORDER_RANDOM
}
