// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

package org.logicng.explanations;

import org.logicng.propositions.Proposition;

import java.util.List;
import java.util.Objects;

/**
 * An unsatisfiable core (can be a minimal unsatisfiable sub-formula).
 * @param <T> the type of the core's propositions
 * @version 1.3
 * @since 1.1
 */
public final class UNSATCore<T extends Proposition> {

    private final List<T> propositions;
    private final boolean isMUS;

    /**
     * Constructs a new unsatisfiable core.
     * @param propositions the propositions of the core
     * @param isMUS        {@code true} if it is a MUS and {@code false} if it is unknown whether it is a MUS.
     */
    public UNSATCore(final List<T> propositions, final boolean isMUS) {
        this.propositions = propositions;
        this.isMUS = isMUS;
    }

    /**
     * Returns the propositions of this MUS.
     * @return the propositions of this MUS
     */
    public List<T> propositions() {
        return this.propositions;
    }

    /**
     * Returns {@code true} if this core is a MUS and {@code false} if it is unknown whether it is a MUS.
     * Note, if set to {@code false} this core might be a MUS, but it is not yet verified.
     * @return {@code true} if this core is a MUS and {@code false} if it is unknown whether it is a MUS.
     */
    public boolean isMUS() {
        return this.isMUS;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.propositions, this.isMUS);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof UNSATCore)) {
            return false;
        }
        final UNSATCore<?> unsatCore = (UNSATCore<?>) o;
        return this.isMUS == unsatCore.isMUS && Objects.equals(this.propositions, unsatCore.propositions);
    }

    @Override
    public String toString() {
        return String.format("UNSATCore{isMUS=%s, propositions=%s}", this.isMUS, this.propositions);
    }
}
