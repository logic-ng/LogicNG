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
 * Open-WBO -- Copyright (c) 2013-2015, Ruben Martins, Vasco Manquinho, Ines Lynce
 * <p>
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 * <p>
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * <p>
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.logicng.solvers.datastructures;

import org.logicng.collections.LNGIntVector;

/**
 * A soft clause for the MaxSAT solver.
 * @version 1.0
 * @since 1.0
 */
public final class MSSoftClause {

    private final LNGIntVector clause;
    private final LNGIntVector relaxationVars;
    private int weight;
    private int assumptionVar;

    /**
     * Constructs a new soft clause.
     * @param clause         the clause
     * @param weight         the weight of this clause
     * @param assumptionVar  the assumption variables of this clause
     * @param relaxationVars the relaxation variables
     */
    public MSSoftClause(final LNGIntVector clause, final int weight, final int assumptionVar, final LNGIntVector relaxationVars) {
        this.clause = new LNGIntVector(clause);
        this.weight = weight;
        this.assumptionVar = assumptionVar;
        this.relaxationVars = new LNGIntVector(relaxationVars);
    }

    /**
     * Returns the clause of this soft clause.
     * @return the clause
     */
    public LNGIntVector clause() {
        return this.clause;
    }

    /**
     * Returns the weight of this soft clause.
     * @return the weight
     */
    public int weight() {
        return this.weight;
    }

    /**
     * Sets the weight
     * @param weight the weight
     */
    public void setWeight(final int weight) {
        this.weight = weight;
    }

    /**
     * Returns the relaxation variables of this soft clause.
     * @return the relaxation variables
     */
    public LNGIntVector relaxationVars() {
        return this.relaxationVars;
    }

    /**
     * Returns the assumption variable.
     * @return the assumption variable
     */
    public int assumptionVar() {
        return this.assumptionVar;
    }

    /**
     * Sets the assumption variable.
     * @param assumptionVar the assumption variable
     */
    public void setAssumptionVar(final int assumptionVar) {
        this.assumptionVar = assumptionVar;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder(String.format("MSSoftClause{weight=%d, assumption=%d lits=[", this.weight, this.assumptionVar));
        for (int i = 0; i < this.clause.size(); i++) {
            final int lit = this.clause.get(i);
            sb.append((lit & 1) == 1 ? "-" : "").append(lit >> 1);
            if (i != this.clause.size() - 1) {
                sb.append(", ");
            }
        }
        sb.append("] relax[");
        for (int i = 0; i < this.relaxationVars.size(); i++) {
            final int lit = this.relaxationVars.get(i);
            sb.append((lit & 1) == 1 ? "-" : "").append(lit >> 1);
            if (i != this.relaxationVars.size() - 1) {
                sb.append(", ");
            }
        }
        sb.append("]}");
        return sb.toString();
    }
}
