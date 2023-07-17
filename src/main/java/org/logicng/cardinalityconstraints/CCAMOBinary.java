// SPDX-License-Identifier: Apache-2.0
// Copyright 2015-2023 Christoph Zengler
// Copyright 2023-20xx BooleWorks GmbH

/*
 * PBLib -- Copyright (c) 2012-2013 Peter Steinke <p> Permission is hereby
 * granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software
 * without restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies of the
 * Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions: <p> The above copyright notice and this
 * permission notice shall be included in all copies or substantial portions of
 * the Software. <p> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
 * KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
 * EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES
 * OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package org.logicng.cardinalityconstraints;

import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Variable;

/**
 * Encodes that at most one variable is assigned value true. Uses the binary
 * encoding due to Doggett, Frisch, Peugniez, and Nightingale.
 * @version 2.0.0
 * @since 1.1
 */
public final class CCAMOBinary implements CCAtMostOne {

    /**
     * Constructs the binary AMO encoder.
     */
    CCAMOBinary() {
        // intentionally left empty
    }

    @Override
    public void build(final EncodingResult result, final Variable... vars) {
        result.reset();
        final int numberOfBits = (int) Math.ceil(Math.log(vars.length) / Math.log(2));
        final int twoPowNBits = (int) Math.pow(2, numberOfBits);
        final int k = (twoPowNBits - vars.length) * 2;
        final Variable[] bits = new Variable[numberOfBits];
        for (int i = 0; i < numberOfBits; i++) {
            bits[i] = result.newVariable();
        }
        int grayCode;
        int nextGray;
        int i = 0;
        int index = -1;
        while (i < k) {
            index++;
            grayCode = i ^ (i >> 1);
            i++;
            nextGray = i ^ (i >> 1);
            for (int j = 0; j < numberOfBits; ++j) {
                if ((grayCode & (1 << j)) == (nextGray & (1 << j))) {
                    if ((grayCode & (1 << j)) != 0) {
                        result.addClause(vars[index].negate(), bits[j]);
                    } else {
                        result.addClause(vars[index].negate(), bits[j].negate());
                    }
                }
            }
            i++;
        }
        while (i < twoPowNBits) {
            index++;
            grayCode = i ^ (i >> 1);
            for (int j = 0; j < numberOfBits; ++j) {
                if ((grayCode & (1 << j)) != 0) {
                    result.addClause(vars[index].negate(), bits[j]);
                } else {
                    result.addClause(vars[index].negate(), bits[j].negate());
                }
            }
            i++;
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
