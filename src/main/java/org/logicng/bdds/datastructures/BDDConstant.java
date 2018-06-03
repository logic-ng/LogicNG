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

package org.logicng.bdds.datastructures;

import org.logicng.formulas.Constant;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;

import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * A terminal node in a BDD.
 * @version 1.4.0
 * @since 1.4.0
 */
public final class BDDConstant implements BDDNode {

  private final Constant value;

  /**
   * Private constructor.
   * @param value the constant value
   */
  private BDDConstant(final Constant value) {
    this.value = value;
  }

  /**
   * Returns the terminal 0 node.
   * @param f the formula factory
   * @return the terminal 0 node
   */
  public static BDDConstant getFalsumNode(final FormulaFactory f) {
    return new BDDConstant(f.falsum());
  }

  /**
   * Returns the terminal 1 node.
   * @param f the formula factory
   * @return the terminal 1 node
   */
  public static BDDConstant getVerumNode(final FormulaFactory f) {
    return new BDDConstant(f.verum());
  }

  @Override
  public Formula label() {
    return this.value;
  }

  @Override
  public boolean isInnerNode() {
    return false;
  }

  @Override
  public BDDNode low() {
    return null;
  }

  @Override
  public BDDNode high() {
    return null;
  }

  @Override
  public Set<BDDNode> nodes() {
    return new HashSet<BDDNode>(Collections.singletonList(this));
  }

  @Override
  public int hashCode() {
    return this.value.hashCode();
  }

  @Override
  public boolean equals(final Object other) {
    return this == other || other instanceof BDDConstant
            && Objects.equals(this.value, ((BDDConstant) other).value);
  }

  @Override
  public String toString() {
    return "<" + this.value + ">";
  }
}
