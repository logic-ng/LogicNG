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

import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.AMOEncoding;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.CardinalityEncoding;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.IncrementalStrategy;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.PBEncoding;

/**
 * Encoders for cardinality constraints, pseudo Booleans and AMO constraints.
 * @version 1.3
 * @since 1.0
 */
public class Encoder {

  private final CardinalityEncoding cardinalityEncoding;
  private final Ladder ladder;
  private final ModularTotalizer mtotalizer;
  private final Totalizer totalizer;
  private final SequentialWeightCounter swc;
  private IncrementalStrategy incrementalStrategy;
  private PBEncoding pbEncoding;
  private AMOEncoding amoEncoding;

  /**
   * Constructs a new Encoder.
   * @param cardinality the cardinality constraint encoder
   */
  public Encoder(final CardinalityEncoding cardinality) {
    this(IncrementalStrategy.NONE, cardinality, AMOEncoding.LADDER, PBEncoding.SWC);
  }

  /**
   * Constructs a new Encoder.
   * @param incremental the incremental strategy
   * @param cardinality the cardinality constraint encoder
   * @param amo         the AMO constraint encoder
   * @param pb          the pseudo Boolean encoder
   */
  private Encoder(final IncrementalStrategy incremental, final CardinalityEncoding cardinality,
                  final AMOEncoding amo, final PBEncoding pb) {
    this.incrementalStrategy = incremental;
    this.cardinalityEncoding = cardinality;
    this.amoEncoding = amo;
    this.pbEncoding = pb;
    this.ladder = new Ladder();
    this.totalizer = new Totalizer(incremental);
    this.mtotalizer = new ModularTotalizer();
    this.swc = new SequentialWeightCounter();
  }


  /**
   * Returns the cardinality encoding.
   * @return the cardinality encoding
   */
  public CardinalityEncoding cardEncoding() {
    return this.cardinalityEncoding;
  }

  /**
   * Sets the pseudo Boolean encoding.
   * @param enc the pseudo Boolean encoding
   */
  public void setPBEncoding(final PBEncoding enc) {
    this.pbEncoding = enc;
  }

  /**
   * Sets the AMO encoding.
   * @param enc the AMO encoding
   */
  public void setAMOEncoding(final AMOEncoding enc) {
    this.amoEncoding = enc;
  }

  /**
   * Controls the modulo value that is used in the modulo totalizer encoding.
   * @param m the module value
   */
  public void setModulo(int m) {
    this.mtotalizer.setModulo(m);
  }

  /**
   * Sets the incremental strategy for the totalizer encoding.
   * @param incremental the incremental strategy
   */
  public void setIncremental(final IncrementalStrategy incremental) {
    this.incrementalStrategy = incremental;
    this.totalizer.setIncremental(incremental);
  }

  /**
   * Encodes an AMO constraint in the given solver.
   * @param s    the solver
   * @param lits the literals for the constraint
   * @throws IllegalStateException if the AMO encoding is unknown
   */
  public void encodeAMO(final MiniSatStyleSolver s, final LNGIntVector lits) {
    switch (this.amoEncoding) {
      case LADDER:
        this.ladder.encode(s, lits);
        break;
      default:
        throw new IllegalStateException("Unknown AMO encoding: " + this.amoEncoding);
    }
  }

  /**
   * Encodes a cardinality constraint in the given solver.
   * @param s    the solver
   * @param lits the literals for the constraint
   * @param rhs  the right hand side of the constraint
   * @throws IllegalStateException if the cardinality encoding is unknown
   */
  public void encodeCardinality(final MiniSatStyleSolver s, final LNGIntVector lits, int rhs) {
    switch (this.cardinalityEncoding) {
      case TOTALIZER:
        this.totalizer.build(s, lits, rhs);
        if (this.totalizer.hasCreatedEncoding())
          this.totalizer.update(s, rhs);
        break;
      case MTOTALIZER:
        this.mtotalizer.encode(s, lits, rhs);
        break;
      default:
        throw new IllegalStateException("Unknown cardinality encoding: " + this.cardinalityEncoding);
    }
  }

  /**
   * Updates the cardinality constraint.
   * @param s   the solver
   * @param rhs the new right hand side
   * @throws IllegalStateException if the cardinality encoding is unknown
   */
  public void updateCardinality(final MiniSatStyleSolver s, int rhs) {
    switch (this.cardinalityEncoding) {
      case TOTALIZER:
        this.totalizer.update(s, rhs);
        break;
      case MTOTALIZER:
        this.mtotalizer.update(s, rhs);
        break;
      default:
        throw new IllegalStateException("Unknown cardinality encoding: " + this.cardinalityEncoding);
    }
  }

  /**
   * Manages the building of cardinality encodings.  Currently is only used for incremental solving.
   * @param s    the solver
   * @param lits the literals for the constraint
   * @param rhs  the right hand side of the constraint
   * @throws IllegalStateException if the cardinality encoding does not support incrementality
   */
  public void buildCardinality(final MiniSatStyleSolver s, final LNGIntVector lits, int rhs) {
    assert this.incrementalStrategy != IncrementalStrategy.NONE;
    switch (this.cardinalityEncoding) {
      case TOTALIZER:
        this.totalizer.build(s, lits, rhs);
        break;
      default:
        throw new IllegalStateException("Cardinality encoding does not support incrementality: " + this.incrementalStrategy);
    }
  }

  /**
   * Manages the incremental update of cardinality constraints.
   * @param s           the solver
   * @param join        the join literals
   * @param lits        the literals of the constraint
   * @param rhs         the right hand side of the constraint
   * @param assumptions the assumptions
   * @throws IllegalStateException if the cardinality encoding does not support incrementality
   */
  public void incUpdateCardinality(final MiniSatStyleSolver s, final LNGIntVector join, final LNGIntVector lits,
                                   int rhs, final LNGIntVector assumptions) {
    assert this.incrementalStrategy == IncrementalStrategy.ITERATIVE;
    switch (this.cardinalityEncoding) {
      case TOTALIZER:
        if (join.size() > 0)
          this.totalizer.join(s, join, rhs);
        assert lits.size() > 0;
        this.totalizer.update(s, rhs, assumptions);
        break;
      default:
        throw new IllegalArgumentException("Cardinality encoding does not support incrementality: " + this.incrementalStrategy);
    }
  }

  /**
   * Encodes a pseudo-Boolean constraint.
   * @param s      the solver
   * @param lits   the literals of the constraint
   * @param coeffs the coefficients of the constraints
   * @param rhs    the right hand side of the constraint
   * @throws IllegalStateException if the pseudo-Boolean encoding is unknown
   */
  public void encodePB(final MiniSatStyleSolver s, final LNGIntVector lits, final LNGIntVector coeffs, int rhs) {
    switch (this.pbEncoding) {
      case SWC:
        this.swc.encode(s, lits, coeffs, rhs);
        break;
      default:
        throw new IllegalStateException("Unknown pseudo-Boolean encoding: " + this.pbEncoding);
    }
  }

  /**
   * Updates a pseudo-Boolean encoding.
   * @param s   the solver
   * @param rhs the new right hand side
   * @throws IllegalStateException if the pseudo-Boolean encoding is unknown
   */
  public void updatePB(final MiniSatStyleSolver s, int rhs) {
    switch (this.pbEncoding) {
      case SWC:
        this.swc.update(s, rhs);
        break;
      default:
        throw new IllegalStateException("Unknown pseudo-Boolean encoding: " + this.pbEncoding);
    }
  }

  /**
   * Incrementally encodes a pseudo-Boolean constraint.
   * @param s           the solver
   * @param lits        the literals of the constraint
   * @param coeffs      the coefficients of the constraint
   * @param rhs         the right hand size of the constraint
   * @param assumptions the current assumptions
   * @param size        the size
   * @throws IllegalStateException if the pseudo-Boolean encoding is unknown
   */
  public void incEncodePB(final MiniSatStyleSolver s, final LNGIntVector lits, final LNGIntVector coeffs,
                          int rhs, final LNGIntVector assumptions, int size) {
    assert this.incrementalStrategy == IncrementalStrategy.ITERATIVE;
    switch (this.pbEncoding) {
      case SWC:
        this.swc.encode(s, lits, coeffs, rhs, assumptions, size);
        break;
      default:
        throw new IllegalStateException("Unknown pseudo-Boolean encoding: " + this.pbEncoding);
    }
  }

  /**
   * Manages the incremental update of pseudo-Boolean encodings.
   * @param s      the solver
   * @param lits   the literals of the constraint
   * @param coeffs the coefficients of the constraint
   * @param rhs    the new right hand side of the constraint
   * @throws IllegalStateException if the pseudo-Boolean encoding is unknown
   */
  public void incUpdatePB(final MiniSatStyleSolver s, final LNGIntVector lits, final LNGIntVector coeffs, int rhs) {
    assert this.incrementalStrategy == IncrementalStrategy.ITERATIVE;
    switch (this.pbEncoding) {
      case SWC:
        this.swc.updateInc(s, rhs);
        this.swc.join(s, lits, coeffs);
        break;
      default:
        throw new IllegalStateException("Unknown pseudo-Boolean encoding: " + this.pbEncoding);
    }
  }

  /**
   * Manages the incremental update of assumptions.
   * @param assumptions the assumptions
   * @throws IllegalStateException if the pseudo-Boolean encoding is unknown
   */
  public void incUpdatePBAssumptions(final LNGIntVector assumptions) {
    assert this.incrementalStrategy == IncrementalStrategy.ITERATIVE;
    switch (this.pbEncoding) {
      case SWC:
        this.swc.updateAssumptions(assumptions);
        break;
      default:
        throw new IllegalStateException("Unknown pseudo-Boolean encoding: " + this.pbEncoding);
    }
  }

  /**
   * Returns {@code true} if the cardinality encoding was built, {@code false} otherwise.
   * @return {@code true} if the cardinality encoding was built
   */
  public boolean hasCardEncoding() {
    if (this.cardinalityEncoding == CardinalityEncoding.TOTALIZER)
      return this.totalizer.hasCreatedEncoding();
    else if (this.cardinalityEncoding == CardinalityEncoding.MTOTALIZER)
      return this.mtotalizer.hasCreatedEncoding();
    return false;
  }

  /**
   * Returns {@code true} if the pseudo-Boolean encoding was built, {@code false} otherwise.
   * @return {@code true} if the pseudo-Boolean encoding was built
   */
  public boolean hasPBEncoding() {
    return this.pbEncoding == PBEncoding.SWC && this.swc.hasCreatedEncoding();
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
