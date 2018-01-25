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
import org.logicng.solvers.sat.MiniSatStyleSolver;

import static org.logicng.solvers.sat.MiniSatStyleSolver.LIT_UNDEF;
import static org.logicng.solvers.sat.MiniSatStyleSolver.var;

/**
 * Super-class for the different encodings.
 * @version 1.3
 * @since 1.0
 */
public abstract class Encoding {

  protected final LNGIntVector clause;
  boolean hasEncoding;

  /**
   * Constructor.
   */
  Encoding() {
    this.clause = new LNGIntVector();
  }

  /**
   * Adds a unit clause to the given SAT solver.
   * @param s the sat solver
   * @param a the unit literal
   */
  void addUnitClause(final MiniSatStyleSolver s, int a) {
    this.addUnitClause(s, a, LIT_UNDEF);
  }

  /**
   * Adds a unit clause to the given SAT solver.
   * @param s        the sat solver
   * @param a        the unit literal
   * @param blocking the blocking literal
   */
  private void addUnitClause(final MiniSatStyleSolver s, int a, int blocking) {
    assert this.clause.size() == 0;
    assert a != LIT_UNDEF;
    assert var(a) < s.nVars();
    this.clause.push(a);
    if (blocking != LIT_UNDEF)
      this.clause.push(blocking);
    s.addClause(this.clause, null);
    this.clause.clear();
  }

  /**
   * Adds a binary clause to the given SAT solver.
   * @param s the sat solver
   * @param a the first literal
   * @param b the second literal
   */
  void addBinaryClause(final MiniSatStyleSolver s, int a, int b) {
    this.addBinaryClause(s, a, b, LIT_UNDEF);
  }

  /**
   * Adds a binary clause to the given SAT solver.
   * @param s        the sat solver
   * @param a        the first literal
   * @param b        the second literal
   * @param blocking the blocking literal
   */
  void addBinaryClause(final MiniSatStyleSolver s, int a, int b, int blocking) {
    assert this.clause.size() == 0;
    assert a != LIT_UNDEF && b != LIT_UNDEF;
    assert var(a) < s.nVars() && var(b) < s.nVars();
    this.clause.push(a);
    this.clause.push(b);
    if (blocking != LIT_UNDEF)
      this.clause.push(blocking);
    s.addClause(this.clause, null);
    this.clause.clear();
  }

  /**
   * Adds a ternary clause to the given SAT solver.
   * @param s the sat solver
   * @param a the first literal
   * @param b the second literal
   * @param c the third literal
   */
  void addTernaryClause(final MiniSatStyleSolver s, int a, int b, int c) {
    this.addTernaryClause(s, a, b, c, LIT_UNDEF);
  }

  /**
   * Adds a ternary clause to the given SAT solver.
   * @param s        the sat solver
   * @param a        the first literal
   * @param b        the second literal
   * @param c        the third literal
   * @param blocking the blocking literal
   */
  void addTernaryClause(final MiniSatStyleSolver s, int a, int b, int c, int blocking) {
    assert this.clause.size() == 0;
    assert a != LIT_UNDEF && b != LIT_UNDEF && c != LIT_UNDEF;
    assert var(a) < s.nVars() && var(b) < s.nVars() && var(c) < s.nVars();
    this.clause.push(a);
    this.clause.push(b);
    this.clause.push(c);
    if (blocking != LIT_UNDEF)
      this.clause.push(blocking);
    s.addClause(this.clause, null);
    this.clause.clear();
  }

  /**
   * Adds a quaterary clause to the given SAT solver.
   * @param s the sat solver
   * @param a the first literal
   * @param b the second literal
   * @param c the third literal
   * @param d the fourth literal
   */
  void addQuaternaryClause(final MiniSatStyleSolver s, int a, int b, int c, int d) {
    this.addQuaternaryClause(s, a, b, c, d, LIT_UNDEF);
  }

  /**
   * Adds a quaterary clause to the given SAT solver.
   * @param s        the sat solver
   * @param a        the first literal
   * @param b        the second literal
   * @param c        the third literal
   * @param d        the fourth literal
   * @param blocking the blocking literal
   */
  private void addQuaternaryClause(final MiniSatStyleSolver s, int a, int b, int c, int d, int blocking) {
    assert this.clause.size() == 0;
    assert a != LIT_UNDEF && b != LIT_UNDEF && c != LIT_UNDEF && d != LIT_UNDEF;
    assert var(a) < s.nVars() && var(b) < s.nVars() && var(c) < s.nVars() && var(d) < s.nVars();
    this.clause.push(a);
    this.clause.push(b);
    this.clause.push(c);
    this.clause.push(d);
    if (blocking != LIT_UNDEF)
      this.clause.push(blocking);
    s.addClause(this.clause, null);
    this.clause.clear();
  }
}
