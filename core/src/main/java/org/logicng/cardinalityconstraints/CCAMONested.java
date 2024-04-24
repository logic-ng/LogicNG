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
 * Encodes that at most one variable is assigned value true.  Uses the nested encoding.
 * @version 2.0.0
 * @since 1.1
 */
public final class CCAMONested implements CCAtMostOne {

    private final int groupSize;
    private EncodingResult result;

    /**
     * Constructs the nested AMO encoder.
     * @param groupSize the group size
     */
    CCAMONested(final int groupSize) {
        this.groupSize = groupSize;
    }

    @Override
    public void build(final EncodingResult result, final Variable... vars) {
        result.reset();
        this.result = result;
        this.encodeIntern(new LNGVector<>(vars));
    }

    /**
     * Internal recursive encoding.
     * @param vars the variables of the constraint
     */
    private void encodeIntern(final LNGVector<Literal> vars) {
        if (vars.size() <= this.groupSize) {
            for (int i = 0; i + 1 < vars.size(); i++) {
                for (int j = i + 1; j < vars.size(); j++) {
                    this.result.addClause(vars.get(i).negate(), vars.get(j).negate());
                }
            }
        } else {
            final LNGVector<Literal> l1 = new LNGVector<>(vars.size() / 2);
            final LNGVector<Literal> l2 = new LNGVector<>(vars.size() / 2);
            int i = 0;
            for (; i < vars.size() / 2; i++) {
                l1.push(vars.get(i));
            }
            for (; i < vars.size(); i++) {
                l2.push(vars.get(i));
            }
            final Variable newVariable = this.result.newVariable();
            l1.push(newVariable);
            l2.push(newVariable.negate());
            this.encodeIntern(l1);
            this.encodeIntern(l2);
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
