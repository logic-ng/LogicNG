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
//  Copyright 2015-2018 Christoph Zengler                                //
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

/*****************************************************************************************
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
 *****************************************************************************************/

package org.logicng.solvers.maxsat.encodings;

import org.logicng.collections.LNGIntVector;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import static org.logicng.solvers.sat.MiniSatStyleSolver.mkLit;
import static org.logicng.solvers.sat.MiniSatStyleSolver.not;

/**
 * Encodes that exactly one literal from 'lits' is assigned value true.  Uses the Ladder/Regular encoding for
 * translating the AMO constraint into CNF.
 * @version 1.1
 * @since 1.0
 */
final class Ladder extends Encoding {

  /**
   * Encodes and adds the AMO constraint to the given solver.
   * @param s    the solver
   * @param lits the literals for the constraint
   */
  public void encode(final MiniSatStyleSolver s, final LNGIntVector lits) {
    assert lits.size() != 0;
    if (lits.size() == 1)
      addUnitClause(s, lits.get(0));
    else {
      final LNGIntVector seqAuxiliary = new LNGIntVector();
      for (int i = 0; i < lits.size() - 1; i++) {
        seqAuxiliary.push(mkLit(s.nVars(), false));
        MaxSAT.newSATVariable(s);
      }
      for (int i = 0; i < lits.size(); i++) {
        if (i == 0) {
          addBinaryClause(s, lits.get(i), not(seqAuxiliary.get(i)));
          addBinaryClause(s, not(lits.get(i)), seqAuxiliary.get(i));
        } else if (i == lits.size() - 1) {
          addBinaryClause(s, lits.get(i), seqAuxiliary.get(i - 1));
          addBinaryClause(s, not(lits.get(i)), not(seqAuxiliary.get(i - 1)));
        } else {
          addBinaryClause(s, not(seqAuxiliary.get(i - 1)), seqAuxiliary.get(i));
          addTernaryClause(s, lits.get(i), not(seqAuxiliary.get(i)), seqAuxiliary.get(i - 1));
          addBinaryClause(s, not(lits.get(i)), seqAuxiliary.get(i));
          addBinaryClause(s, not(lits.get(i)), not(seqAuxiliary.get(i - 1)));
        }
      }
    }
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
