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
 * A variable of the SAT solver for CleaneLing-style solvers.
 * @version 1.0
 * @since 1.0
 */
public final class CLVar {

  /**
   * The state of the variable which has three different states: free, fixed or eliminated.
   */
  public enum State {
    FREE,
    FIXED,
    ELIMINATED
  }

  private State state;
  private int level;
  private int mark;
  private CLClause reason;

  /**
   * Constructs a new variable.
   */
  public CLVar() {
    this.reset();
  }

  /**
   * Resets this variable's members.
   */
  public void reset() {
    this.state = State.FREE;
    this.level = Integer.MAX_VALUE;
    this.mark = 0;
    this.reason = null;
  }

  /**
   * Returns {@code true} if this variable's state is free, {@code false} otherwise.
   * @return {@code true} if this variable's state is free
   */
  public boolean free() {
    return this.state == State.FREE;
  }

  /**
   * Returns the mark value of this variable.
   * @return the mark value of this variable
   */
  public int mark() {
    return this.mark;
  }

  /**
   * Sets the mark value of this variable.
   * @param mark the mark value
   */
  public void setMark(int mark) {
    this.mark = mark;
  }

  /**
   * Returns the decision level of this variable.
   * @return the decision level of this variable
   */
  public int level() {
    return this.level;
  }

  /**
   * Sets the decision level of this variable.
   * @param level the decision level
   */
  public void setLevel(int level) {
    this.level = level;
  }

  /**
   * Returns the state of this variable.
   * @return the state of this variable
   */
  public State state() {
    return this.state;
  }

  /**
   * Sets the state of this variable.
   * @param state the state of this variable
   */
  public void setState(final State state) {
    this.state = state;
  }

  /**
   * Returns the reason for this variable.
   * @return the reason for this variable
   */
  public CLClause reason() {
    return this.reason;
  }

  /**
   * Sets the reason for this variable.
   * @param reason the reason
   */
  public void setReason(final CLClause reason) {
    this.reason = reason;
  }

  @Override
  public String toString() {
    return String.format("CLVar{state=%s, level=%s, mark=%s, reason=%s}", this.state, this.level, this.mark, this.reason);
  }
}

