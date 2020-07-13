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

import org.logicng.datastructures.Tristate;

import java.util.Locale;

/**
 * A variable of the SAT solver for MiniSAT-style solvers.
 * @version 2.0.0
 * @since 1.0
 */
public final class MSVariable {
    private Tristate assignment;
    private int level;
    private MSClause reason;
    private double activity;
    private boolean polarity;
    private boolean decision;

    /**
     * Constructs a new variable with a given initial polarity.
     * @param polarity the initial polarity
     */
    public MSVariable(final boolean polarity) {
        this.assignment = Tristate.UNDEF;
        this.level = -1;
        this.reason = null;
        this.activity = 0;
        this.polarity = polarity;
        this.decision = false;
    }

    /**
     * Sets the decision level of this variable.
     * @param level the decision level
     */
    public void setLevel(final int level) {
        this.level = level;
    }

    /**
     * Returns the decision level of this variable.
     * @return the decision level of this variable
     */
    public int level() {
        return this.level;
    }

    /**
     * Sets the reason for this variable.
     * @param reason the reason for this variable
     */
    public void setReason(final MSClause reason) {
        this.reason = reason;
    }

    /**
     * Returns the reason for this variable.
     * @return the reason for this variable
     */
    public MSClause reason() {
        return this.reason;
    }

    /**
     * Assigns this variable to a given lifted Boolean.
     * @param assignment the lifted Boolean
     */
    public void assign(final Tristate assignment) {
        this.assignment = assignment;
    }

    /**
     * Returns the current assignment of this variable.
     * @return the current assignment of this variable
     */
    public Tristate assignment() {
        return this.assignment;
    }

    /**
     * Rescales this variable's activity.
     */
    public void rescaleActivity() {
        this.activity *= 1e-100;
    }

    /**
     * Increments this variable's activity by a given value
     * @param inc the increment value
     */
    public void incrementActivity(final double inc) {
        this.activity += inc;
    }

    /**
     * Returns the activity of this variable.
     * @return the activity of this variable
     */
    public double activity() {
        return this.activity;
    }

    /**
     * Sets the polarity of this variable.
     * @param polarity the polarity of this variable
     */
    public void setPolarity(final boolean polarity) {
        this.polarity = polarity;
    }

    /**
     * Returns the polarity of this variable.
     * @return the polarity of this variable
     */
    public boolean polarity() {
        return this.polarity;
    }

    /**
     * Returns {@code true} if this variable should be used as a decision variable during solving, {@code false}
     * otherwise.
     * @return {@code true} if this variable should be used as a decision variable
     */
    public boolean decision() {
        return this.decision;
    }

    /**
     * Sets whether this variable can be used as a decision variable during solving or not.
     * @param decision {@code true} if it can be used as decision variable, {@code false} otherwise
     */
    public void setDecision(final boolean decision) {
        this.decision = decision;
    }

    @Override
    public String toString() {
        return String.format(Locale.ENGLISH, "MSVariable{assignment=%s, level=%d, reason=%s, activity=%f, polarity=%s, decision=%s}",
                this.assignment, this.level, this.reason, this.activity, this.polarity, this.decision);
    }
}
