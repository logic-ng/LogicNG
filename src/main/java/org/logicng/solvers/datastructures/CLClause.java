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

import org.logicng.collections.LNGIntVector;

import java.util.Comparator;

/**
 * A clause of the SAT solver for CleaneLing-style solvers.
 * @version 1.3
 * @since 1.0
 */
public final class CLClause {

  /**
   * A comparator for clauses based on glue and activity.
   */
  public static final Comparator<CLClause> comp = new Comparator<CLClause>() {
    @Override
    public int compare(final CLClause c1, CLClause c2) {
      if (c1.glue < c2.glue)
        return -1;
      if (c1.glue > c2.glue)
        return 1;
      return Long.compare(c2.activity, c1.activity);
    }
  };
  private final LNGIntVector lits;
  private int glue;
  private boolean redundant;
  private boolean remove;
  private boolean important;
  private boolean forcing;
  private boolean dumped;
  private boolean satisfied;
  private long activity;

  /**
   * Constructs a new clause with default size 2.
   */
  public CLClause() {
    this.lits = new LNGIntVector(2);
  }

  /**
   * Returns the size (number of literals) of this clause.
   * @return the size of this clause
   */
  public int size() {
    return this.lits.size();
  }

  /**
   * Returns {@code true} if this is a large clause, i.e. has more than two literals, {@code false} otherwise.
   * @return {@code true} if this is a large clause
   */
  public boolean large() {
    return this.lits.size() > 2;
  }

  /**
   * Returns the literals of this clause.
   * @return the literals of this clause
   */
  public LNGIntVector lits() {
    return this.lits;
  }

  /**
   * Returns {@code true} if this clause is currently satisfied, {@code false} otherwise.
   * @return {@code true} if this clause is currently satisfied
   */
  public boolean satisfied() {
    return this.satisfied;
  }

  /**
   * Sets the 'satisfied' status of this clause.
   * @param satisfied {@code true} if this clause is satisfied, {@code false} otherwise
   */
  public void setSatisfied(boolean satisfied) {
    this.satisfied = satisfied;
  }

  /**
   * Returns {@code true} if this clause is the reason for a literal, {@code false} otherwise.
   * @return {@code true} if this clause is the reason for a literal
   */
  public boolean forcing() {
    return this.forcing;
  }

  /**
   * Sets whether this clause is the reason for a literal.
   * @param forcing {@code true} if it is the reason for literal, {@code false} otherwise
   */
  public void setForcing(boolean forcing) {
    this.forcing = forcing;
  }

  /**
   * Returns {@code true} if this clause is important, i.e. its glue value is smaller than a certain threshold,
   * {@code false} otherwise.
   * @return {@code true} if this clause is important
   */
  public boolean important() {
    return this.important;
  }

  /**
   * Sets whether this clause is important or not.
   * @param important {@code true} if it is important, {@code false} otherwise
   */
  public void setImportant(boolean important) {
    this.important = important;
  }

  /**
   * Returns the glue value of this clause.
   * @return the glue value of this clause
   */
  public int glue() {
    return this.glue;
  }

  /**
   * Sets the glue value of this clause.
   * @param glue the glue value
   */
  public void setGlue(int glue) {
    this.glue = glue;
  }

  /**
   * Returns the activity of this clause.
   * @return the activity of this clause
   */
  public long activity() {
    return this.activity;
  }

  /**
   * Sets the activity of this clause.
   * @param activity the activity
   */
  public void setActivity(long activity) {
    this.activity = activity;
  }

  /**
   * Returns {@code true} if this clause is redundant, i.e. learnt, {@code false} otherwise.
   * @return {@code true} if this clause is redundant
   */
  public boolean redundant() {
    return this.redundant;
  }

  /**
   * Sets whether this clause is redundant or not.
   * @param redundant {@code true} if it is important, {@code false} otherwise.
   */
  public void setRedundant(boolean redundant) {
    this.redundant = redundant;
  }

  /**
   * Returns {@code true} if this clause is dumped but not collected, {@code false} otherwise.
   * @return {@code true} if this clause is dumped but not collected
   */
  public boolean dumped() {
    return this.dumped;
  }

  /**
   * Sets whether this clause is dumped or not.
   * @param dumped {@code true} if it is dumped, {@code false} otherwise
   */
  public void setDumped(boolean dumped) {
    this.dumped = dumped;
  }

  /**
   * Returns {@code true} if this clause should be removed during reduction, {@code false} otherwise.
   * @return {@code true} if this clause should be removed during reduction
   */
  public boolean remove() {
    return this.remove;
  }

  /**
   * Sets whether this clause should be removed during reduction or not.
   * @param remove {@code true} if it should be removed, {@code false} otherwise
   */
  public void setRemove(boolean remove) {
    this.remove = remove;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("CLClause{");
    sb.append("glue=").append(this.glue).append(", ");
    sb.append("redundant=").append(this.redundant).append(", ");
    sb.append("remove=").append(this.remove).append(", ");
    sb.append("important=").append(this.important).append(", ");
    sb.append("forcing=").append(this.forcing).append(", ");
    sb.append("dumped=").append(this.dumped).append(", ");
    sb.append("satisfied=").append(this.satisfied).append(", ");
    sb.append("activity=").append(this.activity).append(", ");
    sb.append("lits=[");
    for (int i = 0; i < this.lits.size(); i++) {
      sb.append(this.lits.get(i));
      if (i != this.lits.size() - 1)
        sb.append(", ");
    }
    sb.append("]}");
    return sb.toString();
  }
}
