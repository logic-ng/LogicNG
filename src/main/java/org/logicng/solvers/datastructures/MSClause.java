///////////////////////////////////////////////////////////////////////////
//                   __                _      _   ________               //
//                  / /   ____  ____ _(_)____/ | / / ____/               //
//                 / /   / __ \/ __ `/ / ___/  |/ / / __                 //
//                / /___/ /_/ / /_/ / / /__/ /|  / /_/ /                 //
//               /_____/\____/\__, /_/\___/_/ |_/\____/                  //
//                           /____/                                      //
//                                                                       //
//               The Next Generation Logic Library                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  Copyright 2015-20xx Christoph Zengler                                //
//                                                                       //
//  Licensed under the Apache License, Version 2.0 (the "License");      //
//  you may not use this file except in compliance with the License.     //
//  You may obtain a copy of the License at                              //
//                                                                       //
//  http://www.apache.org/licenses/LICENSE-2.0                           //
//                                                                       //
//  Unless required by applicable law or agreed to in writing, software  //
//  distributed under the License is distributed on an "AS IS" BASIS,    //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      //
//  implied.  See the License for the specific language governing        //
//  permissions and limitations under the License.                       //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

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

import org.logicng.collections.LNGIntVector;

import java.util.Comparator;

/**
 * A clause of the SAT solver for MiniSAT-style solvers.
 * @version 2.0.0
 * @since 1.0
 */
public final class MSClause {

    /**
     * A comparator for clauses based on LBD and activity (used for the Glucose solver).
     */
    public static final Comparator<MSClause> glucoseComparator = (x, y) -> {
        if (x.size() > 2 && y.size() == 2) {
            return -1;
        }
        if (y.size() > 2 && x.size() == 2) {
            return 1;
        }
        if (x.size() == 2 && y.size() == 2) {
            return 1;
        }
        if (x.lbd() > y.lbd()) {
            return -1;
        }
        if (x.lbd() < y.lbd()) {
            return 1;
        }
        return x.activity() < y.activity() ? -1 : 1;
    };

    /**
     * A comparator for clauses based on activity (used for the MiniSAT solver).
     */
    public static final Comparator<MSClause> minisatComparator = (x, y) -> x.size() > 2 && (y.size() == 2 || x.activity() < y.activity()) ? -1 : 1;

    private final LNGIntVector data;
    private final boolean learnt;
    private final boolean isAtMost;
    private double activity;
    private int szWithoutSelectors;
    private boolean seen;
    private long lbd;
    private boolean canBeDel;
    private boolean oneWatched;
    private int atMostWatchers;

    /**
     * Constructs a new clause
     * @param ps     the vector of literals
     * @param learnt {@code true} if it is a learnt clause, {@code false} otherwise
     */
    public MSClause(final LNGIntVector ps, final boolean learnt) {
        this(ps, learnt, false);
    }

    /**
     * Constructs a new clause
     * @param ps       the vector of literals
     * @param learnt   {@code true} if it is a learnt clause, {@code false} otherwise
     * @param isAtMost {@code true} if it is an at-most clause, {@code false} otherwise
     */
    public MSClause(final LNGIntVector ps, final boolean learnt, final boolean isAtMost) {
        this.data = new LNGIntVector(ps.size());
        for (int i = 0; i < ps.size(); i++) {
            this.data.unsafePush(ps.get(i));
        }
        this.learnt = learnt;
        this.szWithoutSelectors = 0;
        this.seen = false;
        this.lbd = 0;
        this.canBeDel = true;
        this.oneWatched = false;
        this.isAtMost = isAtMost;
        this.atMostWatchers = -1;
    }

    /**
     * Returns the size (number of literals) of this clause.
     * @return the size
     */
    public int size() {
        return this.data.size();
    }

    /**
     * Returns the literal at index {@code i}.
     * @param i the index
     * @return the literal at index {@code i}
     */
    public int get(final int i) {
        return this.data.get(i);
    }

    /**
     * Sets the literal at index {@code i}.
     * @param i   the index
     * @param lit the literal
     */
    public void set(final int i, final int lit) {
        this.data.set(i, lit);
    }

    /**
     * Returns the activity of this clause.
     * @return the activity of this clause
     */
    public double activity() {
        return this.activity;
    }

    /**
     * Increments this clause's activity by a given value
     * @param inc the increment value
     */
    public void incrementActivity(final double inc) {
        this.activity += inc;
    }

    /**
     * Rescales this clause's activity
     */
    public void rescaleActivity() {
        this.activity *= 1e-20;
    }

    /**
     * Returns {@code true} if this clause is learnt, {@code false} otherwise.
     * @return {@code true} if this clause is learnt
     */
    public boolean learnt() {
        return this.learnt;
    }

    /**
     * Returns the size of this clause without selector variables.
     * @return the size of this clause without selector variables
     */
    public int sizeWithoutSelectors() {
        return this.szWithoutSelectors;
    }

    /**
     * Sets the size of this clause without selector variables.
     * @param szWithoutSelectors the size of this clause without selector variables
     */
    public void setSizeWithoutSelectors(final int szWithoutSelectors) {
        this.szWithoutSelectors = szWithoutSelectors;
    }

    /**
     * Returns {@code true} if this clause is marked 'seen', {@code false} otherwise.
     * @return {@code true} if this clause is marked 'seen'
     */
    public boolean seen() {
        return this.seen;
    }

    /**
     * Marks this clause with the given 'seen' flag.
     * @param seen the 'seen' flag
     */
    public void setSeen(final boolean seen) {
        this.seen = seen;
    }

    /**
     * Returns the LBD of this clause.
     * @return the LBD of this clause
     */
    public long lbd() {
        return this.lbd;
    }

    /**
     * Sets the LBD of this clause.
     * @param lbd the LBD of this clause
     */
    public void setLBD(final long lbd) {
        this.lbd = lbd;
    }

    /**
     * Returns {@code true} if this clause can be deleted, {@code false} otherwise.
     * @return {@code true} if this clause can be deleted
     */
    public boolean canBeDel() {
        return this.canBeDel;
    }

    /**
     * Sets whether this clause can be deleted or not.
     * @param canBeDel {@code true} if it can be deleted, {@code false} otherwise
     */
    public void setCanBeDel(final boolean canBeDel) {
        this.canBeDel = canBeDel;
    }

    /**
     * Returns {@code true} if this clause is a one literal watched clause, {@code false} otherwise
     * @return {@code true} if this clause is a one literal watched clause
     */
    public boolean oneWatched() {
        return this.oneWatched;
    }

    /**
     * Sets whether this clause is a one literal watched clause or not.
     * @param oneWatched {@code true} if it is a one literal watched clause, {@code false} otherwise
     */
    public void setOneWatched(final boolean oneWatched) {
        this.oneWatched = oneWatched;
    }

    /**
     * Returns {@code true} if this is an at-most clause, {@code false} otherwise.
     * @return {@code true} if this is an at-most clause
     */
    public boolean isAtMost() {
        return this.isAtMost;
    }

    /**
     * Returns the number of watchers if this is an at-most clause.
     * @return the number of watchers
     */
    public int atMostWatchers() {
        assert this.isAtMost;
        return this.atMostWatchers;
    }

    /**
     * Sets the number of watchers for this at-most clause.
     * @param atMostWatchers the number of watchers
     */
    public void setAtMostWatchers(final int atMostWatchers) {
        assert this.isAtMost;
        this.atMostWatchers = atMostWatchers;
    }

    /**
     * Pops (removes) the last literal of this clause.
     */
    public void pop() {
        this.data.pop();
    }

    @Override
    public int hashCode() {
        return this.data.hashCode();
    }

    @Override
    public boolean equals(final Object o) {
        return this == o;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("MSClause{");
        sb.append("activity=").append(this.activity).append(", ");
        sb.append("learnt=").append(this.learnt).append(", ");
        sb.append("szWithoutSelectors=").append(this.szWithoutSelectors).append(", ");
        sb.append("seen=").append(this.seen).append(", ");
        sb.append("lbd=").append(this.lbd).append(", ");
        sb.append("canBeDel=").append(this.canBeDel).append(", ");
        sb.append("oneWatched=").append(this.oneWatched).append(", ");
        sb.append("isAtMost=").append(this.isAtMost).append(", ");
        sb.append("atMostWatchers=").append(this.atMostWatchers).append(", ");
        sb.append("lits=[");
        for (int i = 0; i < this.data.size(); i++) {
            final int lit = this.data.get(i);
            sb.append((lit & 1) == 1 ? "-" : "").append(lit >> 1);
            if (i != this.data.size() - 1) {
                sb.append(", ");
            }
        }
        sb.append("]}");
        return sb.toString();
    }
}
