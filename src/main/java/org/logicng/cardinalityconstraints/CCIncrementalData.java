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
//  Copyright 2015-2016 Christoph Zengler                                //
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

/**
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
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;

import java.util.ArrayList;
import java.util.List;

/**
 * Incremental data for an at-most-k cardinality constraint.  When an at-most-k cardinality constraint is constructed,
 * it is possible to save incremental data with it.  Then one can modify the constraint after it is was created by
 * tightening the original bound.
 * @version 1.1
 * @since 1.1
 */
public final class CCIncrementalData {

  private final FormulaFactory f;
  private final CCConfig.AMK_ENCODER encoder;
  private final LNGVector<? extends Literal> vector1;
  private final LNGVector<? extends Literal> vector2;
  private final int mod;

  /**
   * Constructs new incremental data for the given internal data.
   * @param f       the formula factory
   * @param encoder the at-most-one encoder
   * @param vector1 the first internal vector
   * @param vector2 the second internal vector
   * @param mod     the modulo value
   */
  CCIncrementalData(final FormulaFactory f, CCConfig.AMK_ENCODER encoder, LNGVector<? extends Literal> vector1, LNGVector<? extends Literal> vector2, int mod) {
    this.f = f;
    this.encoder = encoder;
    this.vector1 = vector1;
    this.vector2 = vector2;
    this.mod = mod;
  }

  /**
   * Constructs new incremental data for the given internal data.
   * @param f       the formula factory
   * @param encoder the at-most-one encoder
   * @param vector1 the first internal vector
   */
  CCIncrementalData(final FormulaFactory f, CCConfig.AMK_ENCODER encoder, LNGVector<? extends Literal> vector1) {
    this(f, encoder, vector1, null, -1);
  }

  /**
   * Tightens the upper bound of this constraint and returns the resulting formula.
   * @param rhs the new upperBound
   * @return the incremental encoding of the new upper bound
   */
  public List<Formula> newUpperBound(int rhs) {
    final List<Formula> result = new ArrayList<>();
    switch (encoder) {
      case MODULAR_TOTALIZER:
        assert vector1.size() != 0 || vector2.size() != 0;
        int ulimit = (rhs + 1) / mod;
        int llimit = (rhs + 1) - ulimit * mod;
        assert ulimit <= vector1.size();
        assert llimit <= vector2.size();
        for (int i = ulimit; i < vector1.size(); i++)
          result.add(vector1.get(i).negate());
        if (ulimit != 0 && llimit != 0) {
          for (int i = llimit - 1; i < vector2.size(); i++)
            result.add(f.clause(vector1.get(ulimit - 1).negate(), vector2.get(i).negate()));
        } else {
          if (ulimit == 0) {
            assert llimit != 0;
            for (int i = llimit - 1; i < vector2.size(); i++)
              result.add(vector2.get(i).negate());
          } else
            result.add(vector1.get(ulimit - 1).negate());
        }
        return result;
      case TOTALIZER:
        for (int i = rhs; i < vector1.size(); i++)
          result.add(vector1.get(i).negate());
        return result;
      case CARDINALITY_NETWORK:
        if (vector1.size() > rhs)
          result.add(vector1.get(rhs).negate());
        return result;
      default:
        throw new IllegalStateException("Unknown at-most-k encoder: " + encoder);
    }
  }
}
