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

package org.logicng.pseudobooleans;

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.List;

/**
 * A sequential weight counter for the encoding of pseudo-Boolean constraints in CNF.
 * @version 2.0.0
 * @since 1.0
 */
public final class PBSWC implements PBEncoding {

    private final FormulaFactory f;

    /**
     * Constructs a new sequential weight counter encoder.
     * @param f the formula factory
     */
    PBSWC(final FormulaFactory f) {
        this.f = f;
    }

    @Override
    public List<Formula> encode(final LNGVector<Literal> lits, final LNGIntVector coeffs, final int rhs, final List<Formula> result) {
        this.generateConstraint(rhs, lits, coeffs, result);
        return result;
    }

    private void generateConstraint(final int rhs, final LNGVector<Literal> lits, final LNGIntVector coeffs,
                                    final List<Formula> result) {
        final int n = lits.size();
        final LNGVector<LNGVector<Literal>> seqAuxiliary = new LNGVector<>(n + 1);
        for (int i = 0; i < n + 1; i++) {
            final LNGVector<Literal> temp = new LNGVector<>();
            temp.growTo(rhs + 1);
            seqAuxiliary.push(temp);
        }
        for (int i = 1; i <= n; ++i) {
            for (int j = 1; j <= rhs; ++j) {
                seqAuxiliary.get(i).set(j, this.f.newPBVariable());
            }
        }
        for (int i = 1; i <= n; i++) {
            final int wi = coeffs.get(i - 1);
            assert wi <= rhs;
            for (int j = 1; j <= rhs; j++) {
                if (i >= 2 && i <= n && j <= rhs) {
                    result.add(this.f.clause(seqAuxiliary.get(i - 1).get(j).negate(), seqAuxiliary.get(i).get(j)));
                }
                if (i <= n && j <= wi) {
                    result.add(this.f.clause(lits.get(i - 1).negate(), seqAuxiliary.get(i).get(j)));
                }
                if (i >= 2 && i <= n && j <= rhs - wi) {
                    result.add(this.f.clause(seqAuxiliary.get(i - 1).get(j).negate(), lits.get(i - 1).negate(), seqAuxiliary.get(i).get(j + wi)));
                }
            }
            if (i >= 2) {
                result.add(this.f.clause(seqAuxiliary.get(i - 1).get(rhs + 1 - wi).negate(), lits.get(i - 1).negate()));
            }
        }
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName();
    }
}
