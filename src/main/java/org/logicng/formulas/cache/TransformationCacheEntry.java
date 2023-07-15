// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.formulas.cache;

/**
 * The pre-defined transformation cache entries.
 * @version 2.3.0
 * @since 1.0
 */
public enum TransformationCacheEntry implements CacheEntry {
    NNF("negation normal form"),
    PLAISTED_GREENBAUM_POS("Plaisted & Greenbaum conjunctive normal form (positive polarity)"),
    PLAISTED_GREENBAUM_NEG("Plaisted & Greenbaum conjunctive normal form (negative polarity)"),
    PLAISTED_GREENBAUM_VARIABLE("Plaisted & Greenbaum variable"),
    TSEITIN("Tseitin conjunctive normal form"),
    TSEITIN_VARIABLE("Tseitin variable"),
    FACTORIZED_CNF("factorized conjunctive normal form"),
    BDD_CNF("conjunctive normal form via BDD"),
    FACTORIZED_DNF("factorized disjunctive normal form"),
    BDD_DNF("disjunctive normal form via BDD"),
    AIG("and-inverter graph"),
    UNIT_PROPAGATION("unit propagation"),
    DISTRIBUTIVE_SIMPLIFICATION("distributive simplification"),
    ANONYMIZATION("anonymization");

    private final String description;

    /**
     * Constructs a new entry.
     * @param description the description of this entry
     */
    TransformationCacheEntry(final String description) {
        this.description = description;
    }

    @Override
    public String description() {
        return "TransformationCacheEntry{description=" + this.description + "}";
    }
}
