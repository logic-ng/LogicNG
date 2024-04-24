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
 * <p>
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * <p>
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * <p>
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
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

/**
 * Encodes that at most one variable is assigned value true.  Uses the bimander encoding due to Hölldobler and Nguyen.
 * @version 2.0.0
 * @since 1.1
 */
public final class CCAMOBimander implements CCAtMostOne {

    private final LNGVector<LNGVector<Literal>> groups;
    private final LNGVector<Literal> bits;
    private final int m;
    private EncodingResult result;
    private int numberOfBits;
    private int twoPowNBits;
    private int k;

    /**
     * Constructs the bimander AMO encoder with a given number of groups.
     */
    CCAMOBimander(final int m) {
        this.m = m;
        this.groups = new LNGVector<>();
        this.bits = new LNGVector<>();
    }

    @Override
    public void build(final EncodingResult result, final Variable... vars) {
        result.reset();
        this.result = result;
        this.encodeIntern(new LNGVector<>(vars));
    }

    /**
     * Internal encoding.
     * @param vars the variables of the constraint
     */
    private void encodeIntern(final LNGVector<Literal> vars) {
        this.initializeGroups(vars);
        this.initializeBits();
        int grayCode;
        int nextGray;
        int i = 0;
        int index = -1;
        for (; i < this.k; i++) {
            index++;
            grayCode = i ^ (i >> 1);
            i++;
            nextGray = i ^ (i >> 1);
            for (int j = 0; j < this.numberOfBits; j++) {
                if ((grayCode & (1 << j)) == (nextGray & (1 << j))) {
                    handleGrayCode(grayCode, index, j);
                }
            }
        }
        for (; i < this.twoPowNBits; i++) {
            index++;
            grayCode = i ^ (i >> 1);
            for (int j = 0; j < this.numberOfBits; j++) {
                handleGrayCode(grayCode, index, j);
            }
        }
    }

    private void handleGrayCode(final int grayCode, final int index, final int j) {
        if ((grayCode & (1 << j)) != 0) {
            for (int p = 0; p < this.groups.get(index).size(); ++p) {
                this.result.addClause(this.groups.get(index).get(p).negate(), this.bits.get(j));
            }
        } else {
            for (int p = 0; p < this.groups.get(index).size(); ++p) {
                this.result.addClause(this.groups.get(index).get(p).negate(), this.bits.get(j).negate());
            }
        }
    }

    /**
     * Initializes the groups
     * @param vars the variables of the constraint
     */
    private void initializeGroups(final LNGVector<Literal> vars) {
        final int n = vars.size();
        this.groups.clear();
        for (int i = 0; i < this.m; i++) {
            this.groups.push(new LNGVector<>());
        }

        int g = (int) Math.ceil((double) n / this.m);
        int ig = 0;
        for (int i = 0; i < vars.size(); ) {
            while (i < g) {
                this.groups.get(ig).push(vars.get(i));
                i++;
            }
            ig++;
            g = g + (int) Math.ceil((double) (n - i) / (this.m - ig));
        }

        for (int i = 0; i < this.groups.size(); i++) {
            this.encodeNaive(this.groups.get(i));
        }
    }

    /**
     * Initializes the bits.
     */
    private void initializeBits() {
        this.bits.clear();
        this.numberOfBits = (int) Math.ceil(Math.log(this.m) / Math.log(2));
        this.twoPowNBits = (int) Math.pow(2, this.numberOfBits);
        this.k = (this.twoPowNBits - this.m) * 2;
        for (int i = 0; i < this.numberOfBits; ++i) {
            this.bits.push(this.result.newVariable());
        }
    }

    /**
     * Naive encoding of a cardinality constraint.
     * @param vars the variables of the constraint
     */
    private void encodeNaive(final LNGVector<Literal> vars) {
        if (vars.size() > 1) {
            for (int i = 0; i < vars.size(); i++) {
                for (int j = i + 1; j < vars.size(); j++) {
                    this.result.addClause(vars.get(i).negate(), vars.get(j).negate());
                }
            }
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
