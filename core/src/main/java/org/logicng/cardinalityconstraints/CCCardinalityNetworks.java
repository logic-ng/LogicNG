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

import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.BOTH;
import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.INPUT_TO_OUTPUT;
import static org.logicng.cardinalityconstraints.CCSorting.ImplicationDirection.OUTPUT_TO_INPUT;

import org.logicng.collections.LNGVector;
import org.logicng.datastructures.EncodingResult;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

/**
 * Implementation of cardinality networks due to Asín, Nieuwenhuis, Oliveras, and Rodríguez-Carbonell.
 * @version 2.0.0
 * @since 1.1
 */
public final class CCCardinalityNetworks {

    private final CCSorting sorting;
    private CCIncrementalData incData;

    /**
     * Constructs a new cardinality encoder.
     */
    CCCardinalityNetworks() {
        this.sorting = new CCSorting();
    }

    void buildAMK(final EncodingResult result, final Variable[] vars, final int rhs) {
        result.reset();
        final LNGVector<Literal> input = new LNGVector<>();
        final LNGVector<Literal> output = new LNGVector<>();
        if (rhs > vars.length / 2) {
            final int geq = vars.length - rhs;
            for (final Variable v : vars) {
                input.push(v.negate());
            }
            this.sorting.sort(geq, input, result, output, OUTPUT_TO_INPUT);
            for (int i = 0; i < geq; i++) {
                result.addClause(output.get(i));
            }
        } else {
            for (final Variable v : vars) {
                input.push(v);
            }
            this.sorting.sort(rhs + 1, input, result, output, INPUT_TO_OUTPUT);
            assert output.size() > rhs;
            result.addClause(output.get(rhs).negate());
        }
    }

    void buildAMKForIncremental(final EncodingResult result, final Variable[] vars, final int rhs) {
        final LNGVector<Literal> input = new LNGVector<>();
        final LNGVector<Literal> output = new LNGVector<>();
        for (final Variable var : vars) {
            input.push(var);
        }
        this.sorting.sort(rhs + 1, input, result, output, INPUT_TO_OUTPUT);
        assert output.size() > rhs;
        result.addClause(output.get(rhs).negate());
        this.incData = new CCIncrementalData(result, CCConfig.AMK_ENCODER.CARDINALITY_NETWORK, rhs, output);
    }

    void buildALK(final EncodingResult result, final Variable[] vars, final int rhs) {
        result.reset();
        final LNGVector<Literal> input = new LNGVector<>();
        final LNGVector<Literal> output = new LNGVector<>();
        final int newRHS = vars.length - rhs;
        if (newRHS > vars.length / 2) {
            final int geq = vars.length - newRHS;
            for (final Variable v : vars) {
                input.push(v);
            }
            this.sorting.sort(geq, input, result, output, OUTPUT_TO_INPUT);
            for (int i = 0; i < geq; i++) {
                result.addClause(output.get(i));
            }
        } else {
            for (final Variable v : vars) {
                input.push(v.negate());
            }
            this.sorting.sort(newRHS + 1, input, result, output, INPUT_TO_OUTPUT);
            assert output.size() > newRHS;
            result.addClause(output.get(newRHS).negate());
        }
    }

    void buildALKForIncremental(final EncodingResult result, final Variable[] vars, final int rhs) {
        final LNGVector<Literal> input = new LNGVector<>();
        final LNGVector<Literal> output = new LNGVector<>();
        for (final Variable var : vars) {
            input.push(var.negate());
        }
        final int newRHS = vars.length - rhs;
        this.sorting.sort(newRHS + 1, input, result, output, INPUT_TO_OUTPUT);
        assert output.size() > newRHS;
        result.addClause(output.get(newRHS).negate());
        this.incData = new CCIncrementalData(result, CCConfig.ALK_ENCODER.CARDINALITY_NETWORK, rhs, vars.length, output);
    }

    void buildEXK(final EncodingResult result, final Variable[] vars, final int rhs) {
        result.reset();
        final LNGVector<Literal> input = new LNGVector<>();
        final LNGVector<Literal> output = new LNGVector<>();
        for (final Variable var : vars) {
            input.push(var);
        }
        this.sorting.sort(rhs + 1, input, result, output, BOTH);
        assert output.size() > rhs;
        result.addClause(output.get(rhs).negate());
        result.addClause(output.get(rhs - 1));
    }

    CCIncrementalData incrementalData() {
        return this.incData;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
