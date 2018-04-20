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

  /**
   * Constructs a new term with a given set of bits and the related minterms.
   * @param bits     the bits
   * @param minterms the minterms
   */
  Term(final Tristate[] bits, final List<Formula> minterms) {
    this.bits = bits;
    this.minterms = minterms;
    this.termClass = countNonNegativeBits(bits);
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

  Tristate[] bits() {
    return this.bits;
  }

  List<Formula> minterms() {
    return this.minterms;
  }

  int termClass() {
    return this.termClass;
  }

  boolean isUsed() {
    return this.used;
  }

  void setUsed(final boolean used) {
    this.used = used;
  }

  /**
   * Units this term with another term if possible.  This is only possible if their bit vectors
   * differ in exactly one position.  In this case a new term with the new bit vector and the
   * united minterms is returned.  If no union is possible, {@code null} is returned.
   * @param other the other term
   * @return a new united term or {@code null} if not possible
   */
  Term unite(final Term other) {
    if (this.bits.length != other.bits.length)
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
