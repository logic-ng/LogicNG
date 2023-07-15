// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

/*
 * MiniSat -- Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 * OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.logicng.solvers.datastructures;

/**
 * A watcher for clauses for MiniSAT-style solvers.
 * @version 1.0.1
 * @since 1.0
 */
public final class MSWatcher {
    private final MSClause clause;
    private final int blocker;

    /**
     * Constructs a new watcher.
     * @param clause  the watched clause
     * @param blocker the blocking literal
     */
    public MSWatcher(final MSClause clause, final int blocker) {
        this.clause = clause;
        this.blocker = blocker;
    }

    /**
     * Returns the blocking literal of this watcher.
     * @return the blocking literal of this watcher
     */
    public int blocker() {
        return this.blocker;
    }

    /**
     * Returns the watched clause of this watcher.
     * @return the watched clause of this watcher
     */
    public MSClause clause() {
        return this.clause;
    }

    @Override
    public int hashCode() {
        return this.clause.hashCode();
    }

    @Override
    public boolean equals(final Object other) {
        return this == other || other instanceof MSWatcher && this.clause == (((MSWatcher) other).clause);
    }

    @Override
    public String toString() {
        return String.format("MSWatcher{clause=%s, blocker=%d}", this.clause, this.blocker);
    }
}
