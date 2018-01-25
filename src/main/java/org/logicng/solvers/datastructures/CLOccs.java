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

/**************************************************************************
 * Copyright (C) 2012 - 2014 Armin Biere JKU Linz
 * <p>
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * <p>
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * <p>
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 ****************************************************************************/

package org.logicng.solvers.datastructures;

import org.logicng.collections.LNGVector;

import java.util.Iterator;

/**
 * Full occurrence lists for dense mode of the SAT solver (used during simplifications).
 * @version 1.1
 * @since 1.0
 */
public final class CLOccs implements Iterable<CLClause> {
  private final LNGVector<CLClause> clauses;
  private int count;

  /**
   * Constructs a new occurrence list.
   */
  public CLOccs() {
    this.count = 0;
    this.clauses = new LNGVector<>();
  }

  /**
   * Decrements the number of clauses in this occurrence list.
   */
  public void dec() {
    this.count--;
  }

  /**
   * Adds a new clause to this occurrence list.
   * @param c the clause
   */
  public void add(final CLClause c) {
    this.count++;
    this.clauses.push(c);
  }

  /**
   * Returns the number of clauses in this occurrence list.
   * @return the number of clauses in this occurrence list
   */
  public int count() {
    return this.count;
  }

  /**
   * Returns the clauses of this occurrence list.
   * @return the clauses of this occurrence list
   */
  public LNGVector<CLClause> clauses() {
    return this.clauses;
  }

  /**
   * Releases this occurrence list.
   */
  public void release() {
    this.clauses.release();
    this.count = 0;
  }

  @Override
  public Iterator<CLClause> iterator() {
    return this.clauses.iterator();
  }

  @Override
  public String toString() {
    return String.format("CLOccs{count=%d, clauses=%s}", this.count, this.clauses);
  }
}
