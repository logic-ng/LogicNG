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
 * Encodes that at most one variable is assigned value true.  Uses the commander encoding due to Klieber &amp; Kwon.
 * @version 2.0.0
 * @since 1.1
 */
public final class CCAMOCommander implements CCAtMostOne {

    private final int k;
    private final LNGVector<Literal> literals;
    private final LNGVector<Literal> nextLiterals;
    private final LNGVector<Literal> currentLiterals;
    private EncodingResult result;

    /**
     * Constructs the commander AMO encoder with a given group size.
     * @param k the group size for the encoding
     */
    CCAMOCommander(final int k) {
        this.k = k;
        this.literals = new LNGVector<>();
        this.nextLiterals = new LNGVector<>();
        this.currentLiterals = new LNGVector<>();
    }

    @Override
    public void build(final EncodingResult result, final Variable... vars) {
        result.reset();
        this.result = result;
        this.currentLiterals.clear();
        this.nextLiterals.clear();
        for (final Variable var : vars) {
            this.currentLiterals.push(var);
        }
        this.encodeRecursive();
    }

    /**
     * Internal recursive encoding.
     */
    private void encodeRecursive() {
        boolean isExactlyOne = false;
        while (this.currentLiterals.size() > this.k) {
            this.literals.clear();
            this.nextLiterals.clear();
            for (int i = 0; i < this.currentLiterals.size(); i++) {
                this.literals.push(this.currentLiterals.get(i));
                if (i % this.k == this.k - 1 || i == this.currentLiterals.size() - 1) {
                    this.encodeNonRecursive(this.literals);
                    this.literals.push(this.result.newVariable());
                    this.nextLiterals.push(this.literals.back().negate());
                    if (isExactlyOne && this.literals.size() > 0) {
                        this.result.addClause(this.literals);
                    }
                    for (int j = 0; j < this.literals.size() - 1; j++) {
                        this.result.addClause(this.literals.back().negate(), this.literals.get(j).negate());
                    }
                    this.literals.clear();
                }
            }
            this.currentLiterals.replaceInplace(this.nextLiterals);
            isExactlyOne = true;
        }
        this.encodeNonRecursive(this.currentLiterals);
        if (isExactlyOne && this.currentLiterals.size() > 0) {
            this.result.addClause(this.currentLiterals);
        }
    }

    /**
     * Internal non recursive encoding.
     * @param literals the current literals
     */
    private void encodeNonRecursive(final LNGVector<Literal> literals) {
        if (literals.size() > 1) {
            for (int i = 0; i < literals.size(); i++) {
                for (int j = i + 1; j < literals.size(); j++) {
                    this.result.addClause(literals.get(i).negate(), literals.get(j).negate());
                }
            }
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
