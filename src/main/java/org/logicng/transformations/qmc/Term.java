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

package org.logicng.transformations.qmc;

import org.logicng.datastructures.Tristate;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A term in the Quine–McCluskey algorithm.  A term represents a set of
 * minterms of a canonical DNF and has its own representation as a vector
 * of tristates.  Two minterms are considered equals if their bit representation
 * is equals (independent of their associated minterms)
 * @version 1.4.0
 * @since 1.4.0
 */
class Term {

  private final Tristate[] bits;
  private final List<Formula> minterms;
  private final int termClass;
  private boolean used;
  private final long undefNum;

  /**
   * Constructs a new term with a given set of bits and the related minterms.
   * @param bits     the bits
   * @param minterms the minterms
   */
  Term(final Tristate[] bits, final List<Formula> minterms) {
    this.bits = bits;
    this.minterms = minterms;
    this.termClass = countNonNegativeBits(bits);
    this.undefNum = computeUndefNum(bits);
  }

  /**
   * Counts the number of non-negative bits of a given tristate vector.
   * @param bits the tristate vector
   * @return the number of non-negative bits
   */
  private int countNonNegativeBits(final Tristate[] bits) {
    int result = 0;
    for (final Tristate bit : bits)
      if (bit != Tristate.FALSE)
        result++;
    return result;
  }

  /**
   * Computes a number representing the number and position of the UNDEF states in the bit array.
   * @param bits the bit array
   * @return the computed number
   */
  private long computeUndefNum(final Tristate[] bits) {
    long sum = 0;
    for (int i = bits.length - 1; i >= 0; i--)
      if (bits[i] == Tristate.UNDEF)
        sum += Math.pow(2, bits.length - 1 - i);
    return sum;
  }

  /**
   * Returns the bit array of this term.
   * @return the bit array of this term
   */
  Tristate[] bits() {
    return this.bits;
  }

  /**
   * Returns the associated minterms for this term.
   * @return the associated minterms for this term
   */
  List<Formula> minterms() {
    return this.minterms;
  }

  /**
   * Returns the term class of this term.  The term class is the number of non-negative bits in the bit array.
   * @return the term class of this term
   */
  int termClass() {
    return this.termClass;
  }

  /**
   * Returns whether this term was used in the combination step of QMC or not.
   * @return whether this term was used
   */
  boolean isUsed() {
    return this.used;
  }

  /**
   * Sets whether this term was used in the combination step of QMC or not.
   * @param used whether this term was used
   */
  void setUsed(final boolean used) {
    this.used = used;
  }

  /**
   * Combines this term with another term if possible.  This is only possible if their bit vectors
   * differ in exactly one position.  In this case a new term with the new bit vector and the
   * combined minterms is returned.  If no union is possible, {@code null} is returned.
   * @param other the other term
   * @return a new combined term or {@code null} if not possible
   */
  Term combine(final Term other) {
    if (this.bits.length != other.bits.length)
      return null;
    if (this.undefNum != other.undefNum)
      return null;
    int diffPosition = -1;
    for (int i = 0; i < this.bits.length; i++)
      if (this.bits[i] != other.bits[i]) {
        if (diffPosition != -1)
          return null;
        else
          diffPosition = i;
      }
    if (diffPosition == -1)
      return null;
    final Tristate[] newBits = Arrays.copyOf(this.bits, this.bits.length);
    newBits[diffPosition] = Tristate.UNDEF;
    final List<Formula> newMinterms = new ArrayList<>(this.minterms);
    newMinterms.addAll(other.minterms);
    return new Term(newBits, newMinterms);
  }

  /**
   * Translates this term to a formula for a given variable ordering
   * @param varOrder the variable ordering
   * @return the translation of this term to a formula
   */
  Formula translateToFormula(final List<Variable> varOrder) {
    final FormulaFactory f = varOrder.get(0).factory();
    assert this.bits.length == varOrder.size();
    final List<Literal> operands = new ArrayList<>(varOrder.size());
    for (int i = 0; i < this.bits.length; i++)
      if (this.bits[i] != Tristate.UNDEF)
        operands.add(this.bits[i] == Tristate.TRUE ? varOrder.get(i) : varOrder.get(i).negate());
    return f.and(operands);
  }

  @Override
  public boolean equals(final Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    final Term term = (Term) o;
    return Arrays.equals(this.bits, term.bits);
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(this.bits);
  }

  @Override
  public String toString() {
    return "Term{" +
            "bits=" + Arrays.toString(this.bits) +
            ", minterms=" + this.minterms +
            ", termClass=" + this.termClass +
            '}';
  }
}
