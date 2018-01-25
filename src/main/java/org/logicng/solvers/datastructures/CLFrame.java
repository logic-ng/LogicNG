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

/**
 * A decision frame for the SAT solver.  Each decision opens up a new frame.
 * @version 1.0
 * @since 1.0
 */
public final class CLFrame {
  private final int decision;
  private final int level;
  private final int trail;
  private boolean mark;

  /**
   * Constructs a new frame.
   * @param d the decision at this level
   * @param l the decision level
   * @param t the trail height before this decision
   */
  public CLFrame(int d, int l, int t) {
    this.decision = d;
    this.level = l;
    this.trail = t;
    this.mark = false;
  }

  /**
   * Constructs a new empty frame at level 0.
   */
  public CLFrame() {
    this(0, 0, 0);
  }

  /**
   * Returns {@code true} if this frame is marked, {@code false} otherwise.
   * @return {@code true} if this frame is marked
   */
  public boolean mark() {
    return this.mark;
  }

  /**
   * Sets the 'mark' flag of this frame.
   * @param mark {@code true} if it is marked, {@code false} otherwise
   */
  public void setMark(boolean mark) {
    this.mark = mark;
  }

  /**
   * Returns the decision level of this frame.
   * @return the decision level of this frame
   */
  public int level() {
    return this.level;
  }

  /**
   * Returns the trail height before this decision.
   * @return the trail height before this decision
   */
  public int trail() {
    return this.trail;
  }

  /**
   * Returns the decision of this frame.
   * @return the decision of this frame
   */
  public int decision() {
    return this.decision;
  }

  @Override
  public String toString() {
    return String.format("CLFrame{decision=%d, level=%d, trail=%d, mark=%s}", this.decision, this.level, this.trail, this.mark);
  }
}
