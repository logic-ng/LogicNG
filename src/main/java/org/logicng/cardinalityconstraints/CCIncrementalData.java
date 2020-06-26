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
 * PBLib       -- Copyright (c) 2012-2013  Peter Steinke
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.logicng.cardinalityconstraints;

import org.logicng.collections.LNGVector;
import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;

import java.util.List;

/**
 * Incremental data for an at-most-k cardinality constraint.  When an at-most-k cardinality constraint is constructed,
 * it is possible to save incremental data with it.  Then one can modify the constraint after it is was created by
 * tightening the original bound.
 * @version 1.1
 * @since 1.1
 */
public final class CCIncrementalData {
    private final EncodingResult result;
    private final CCConfig.AMK_ENCODER amkEncoder;
    private final CCConfig.ALK_ENCODER alkEncoder;
    private final LNGVector<? extends Literal> vector1;
    private final LNGVector<? extends Literal> vector2;
    private final int mod;
    private int nVars;
    private int currentRHS;

    /**
     * Constructs new incremental data for an at-most-k encoder and the given internal data.
     * @param result     the result
     * @param amkEncoder the at-most-one amkEncoder
     * @param rhs        the current right-hand-side
     * @param vector1    the first internal vector
     * @param vector2    the second internal vector
     * @param mod        the modulo value
     */
    CCIncrementalData(final EncodingResult result, final CCConfig.AMK_ENCODER amkEncoder, final int rhs,
                      final LNGVector<? extends Literal> vector1, final LNGVector<? extends Literal> vector2, final int mod) {
        this.result = result;
        this.amkEncoder = amkEncoder;
        this.alkEncoder = null;
        this.currentRHS = rhs;
        this.vector1 = vector1;
        this.vector2 = vector2;
        this.mod = mod;
    }

    /**
     * Constructs new incremental data for an at-most-k encoder and the given internal data.
     * @param result  the result
     * @param encoder the at-most-one amkEncoder
     * @param rhs     the current right-hand-side
     * @param vector1 the first internal vector
     */
    CCIncrementalData(final EncodingResult result, final CCConfig.AMK_ENCODER encoder, final int rhs, final LNGVector<? extends Literal> vector1) {
        this(result, encoder, rhs, vector1, null, -1);
    }

    /**
     * Constructs new incremental data for an at-least-k encoder and the given internal data.
     * @param result     the result
     * @param alkEncoder the at-least-one amkEncoder
     * @param rhs        the current right-hand-side
     * @param nVars      the number of variables
     * @param vector1    the first internal vector
     * @param vector2    the second internal vector
     * @param mod        the modulo value
     */
    CCIncrementalData(final EncodingResult result, final CCConfig.ALK_ENCODER alkEncoder, final int rhs, final int nVars,
                      final LNGVector<? extends Literal> vector1, final LNGVector<? extends Literal> vector2, final int mod) {
        this.result = result;
        this.alkEncoder = alkEncoder;
        this.amkEncoder = null;
        this.currentRHS = rhs;
        this.nVars = nVars;
        this.vector1 = vector1;
        this.vector2 = vector2;
        this.mod = mod;

    }

    /**
     * Constructs new incremental data for an at-least-k encoder and the given internal data.
     * @param result     the result
     * @param alkEncoder the at-most-one amkEncoder
     * @param rhs        the current right-hand-side
     * @param nVars      the number of variables
     * @param vector1    the first internal vector
     */
    CCIncrementalData(final EncodingResult result, final CCConfig.ALK_ENCODER alkEncoder, final int rhs, final int nVars, final LNGVector<? extends Literal> vector1) {
        this(result, alkEncoder, rhs, nVars, vector1, null, -1);
    }

    /**
     * Tightens the upper bound of an at-most-k constraint and returns the resulting formula.
     * @param rhs the new upperBound
     * @return the incremental encoding of the new upper bound
     */
    public List<Formula> newUpperBound(final int rhs) {
        this.result.reset();
        this.computeUBConstraint(this.result, rhs);
        return this.result.result();
    }

    /**
     * Tightens the upper bound of an at-most-k constraint and encodes it on the solver of the result.
     * <p>
     * Usage constraints:
     * -New right hand side must be smaller than current right hand side.
     * -Cannot be used for at-least-k constraints.
     * @param rhs the new upperBound
     */
    public void newUpperBoundForSolver(final int rhs) {
        this.computeUBConstraint(this.result, rhs);
    }

    private void computeUBConstraint(final EncodingResult result, final int rhs) {
        if (rhs >= this.currentRHS) {
            throw new IllegalArgumentException("New upper bound " + rhs + " + does not tighten the current bound of " + this.currentRHS);
        }
        this.currentRHS = rhs;
        if (this.amkEncoder == null) {
            throw new IllegalStateException("Cannot encode a new upper bound for an at-most-k constraint");
        }
        switch (this.amkEncoder) {
            case MODULAR_TOTALIZER:
                assert this.vector1.size() != 0 || this.vector2.size() != 0;
                final int ulimit = (rhs + 1) / this.mod;
                final int llimit = (rhs + 1) - ulimit * this.mod;
                assert ulimit <= this.vector1.size();
                assert llimit <= this.vector2.size();
                for (int i = ulimit; i < this.vector1.size(); i++) {
                    result.addClause(this.vector1.get(i).negate());
                }
                if (ulimit != 0 && llimit != 0) {
                    for (int i = llimit - 1; i < this.vector2.size(); i++) {
                        result.addClause(this.vector1.get(ulimit - 1).negate(), this.vector2.get(i).negate());
                    }
                } else {
                    if (ulimit == 0) {
                        assert llimit != 0;
                        for (int i = llimit - 1; i < this.vector2.size(); i++) {
                            result.addClause(this.vector2.get(i).negate());
                        }
                    } else {
                        result.addClause(this.vector1.get(ulimit - 1).negate());
                    }
                }
                break;
            case TOTALIZER:
                for (int i = rhs; i < this.vector1.size(); i++) {
                    result.addClause(this.vector1.get(i).negate());
                }
                break;
            case CARDINALITY_NETWORK:
                if (this.vector1.size() > rhs) {
                    result.addClause(this.vector1.get(rhs).negate());
                }
                break;
            default:
                throw new IllegalStateException("Unknown at-most-k encoder: " + this.amkEncoder);
        }
    }

    /**
     * Tightens the lower bound of an at-least-k constraint and returns the resulting formula.
     * @param rhs the new upperBound
     * @return the incremental encoding of the new lower bound
     */
    public List<Formula> newLowerBound(final int rhs) {
        this.result.reset();
        this.computeLBConstraint(this.result, rhs);
        return this.result.result();
    }

    /**
     * Tightens the lower bound of an at-least-k constraint and encodes it on the solver of the result.
     * <p>
     * Usage constraints:
     * -New right hand side must be greater than current right hand side.
     * -Cannot be used for at-most-k constraints.
     * @param rhs the new upperBound
     */
    public void newLowerBoundForSolver(final int rhs) {
        this.computeLBConstraint(this.result, rhs);
    }

    private void computeLBConstraint(final EncodingResult result, final int rhs) {
        if (rhs <= this.currentRHS) {
            throw new IllegalArgumentException("New lower bound " + rhs + " + does not tighten the current bound of " + this.currentRHS);
        }
        this.currentRHS = rhs;
        if (this.alkEncoder == null) {
            throw new IllegalStateException("Cannot encode a new lower bound for an at-least-k constraint");
        }
        switch (this.alkEncoder) {
            case TOTALIZER:
                for (int i = 0; i < rhs; i++) {
                    result.addClause(this.vector1.get(i));
                }
                break;
            case MODULAR_TOTALIZER:
                int newRHS = this.nVars - rhs;
                assert this.vector1.size() != 0 || this.vector2.size() != 0;
                final int ulimit = (newRHS + 1) / this.mod;
                final int llimit = (newRHS + 1) - ulimit * this.mod;
                assert ulimit <= this.vector1.size();
                assert llimit <= this.vector2.size();
                for (int i = ulimit; i < this.vector1.size(); i++) {
                    result.addClause(this.vector1.get(i).negate());
                }
                if (ulimit != 0 && llimit != 0) {
                    for (int i = llimit - 1; i < this.vector2.size(); i++) {
                        result.addClause(this.vector1.get(ulimit - 1).negate(), this.vector2.get(i).negate());
                    }
                } else {
                    if (ulimit == 0) {
                        assert llimit != 0;
                        for (int i = llimit - 1; i < this.vector2.size(); i++) {
                            result.addClause(this.vector2.get(i).negate());
                        }
                    } else {
                        result.addClause(this.vector1.get(ulimit - 1).negate());
                    }
                }
                break;
            case CARDINALITY_NETWORK:
                newRHS = this.nVars - rhs;
                if (this.vector1.size() > newRHS) {
                    result.addClause(this.vector1.get(newRHS).negate());
                }
                break;
            default:
                throw new IllegalStateException("Unknown at-least-k encoder: " + this.alkEncoder);
        }
    }

    /**
     * Returns the current right hand side of this CCIncrementalData.
     * @return the current right hand side of this CCIncrementalData.
     */
    public int currentRHS() {
        return this.currentRHS;
    }

    @Override
    public String toString() {
        return "CCIncrementalData{" +
                ", amkEncoder=" + this.amkEncoder +
                ", alkEncoder=" + this.alkEncoder +
                ", vector1=" + this.vector1 +
                ", vector2=" + this.vector2 +
                ", mod=" + this.mod +
                ", currentRHS=" + this.currentRHS +
                '}';
    }
}
