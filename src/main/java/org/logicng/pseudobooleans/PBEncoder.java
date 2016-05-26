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
//  Copyright 2015-2016 Christoph Zengler                                //
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

package org.logicng.pseudobooleans;

import org.logicng.cardinalityconstraints.CCConfig;
import org.logicng.cardinalityconstraints.CCEncoder;
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.configurations.Configuration;
import org.logicng.configurations.ConfigurationType;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;

import java.util.LinkedList;
import java.util.List;

/**
 * An encoder for pseudo-Boolean constraints.
 * @version 1.1
 * @since 1.0
 */
public abstract class PBEncoder {

  protected final FormulaFactory f;
  private final CCEncoder ccEncoder;

  /**
   * Constructs a new pseudo-Boolean encoder.
   * @param f the formula factory
   */
  public PBEncoder(final FormulaFactory f) {
    this.f = f;
    Configuration ccConfig = f.configurationFor(ConfigurationType.CC_ENCODER);
    if (ccConfig == null)
      ccConfig = new CCConfig.Builder().build();
    this.ccEncoder = new CCEncoder(f, (CCConfig) ccConfig);
  }

  /**
   * Builds a pseudo Boolean constraint of the form {@code c_1 * lit_1 + c_2 * lit_2 + ... + c_n * lit_n >= k}.
   * @param constraint the constraint
   * @return the CNF encoding of the pseudo Boolean constraint
   * @throws IllegalArgumentException if the right hand side of the cardinality constraint is negative or
   *                                  larger than the number of literals
   */
  public ImmutableFormulaList build(final PBConstraint constraint) {
    if (constraint.isCC())
      return this.ccEncoder.encode(constraint);
    final Formula normalized = constraint.normalize();
    switch (normalized.type()) {
      case TRUE:
        return new ImmutableFormulaList(FType.AND);
      case FALSE:
        return new ImmutableFormulaList(FType.AND, this.f.falsum());
      case PBC:
        final PBConstraint pbc = (PBConstraint) normalized;
        if (pbc.isCC())
          return this.ccEncoder.encode(constraint);
        return this.build(pbc.operands(), pbc.coefficients(), pbc.rhs());
      case AND:
        final List<Formula> list = new LinkedList<>();
        for (final Formula op : normalized) {
          switch (op.type()) {
            case FALSE:
              return new ImmutableFormulaList(FType.AND, this.f.falsum());
            case PBC:
              list.addAll(this.build((PBConstraint) op).toList());
              break;
            default:
              throw new IllegalArgumentException("Illegal return value of PBConstraint.normalize");
          }
        }
        return new ImmutableFormulaList(FType.AND, list);
      default:
        throw new IllegalArgumentException("Illegal return value of PBConstraint.normalize");
    }
  }

  /**
   * Builds a pseudo Boolean constraint of the form {@code c_1 * lit_1 + c_2 * lit_2 + ... + c_n * lit_n >= k}.
   * @param lits   the literals {@code lit_1 ... lit_n}
   * @param coeffs the coefficients {@code c_1 ... c_n}
   * @param rhs    the right hand side {@code k} of the constraint
   * @return the CNF encoding of the pseudo Boolean constraint
   * @throws IllegalArgumentException if the right hand side of the cardinality constraint is negative or
   *                                  larger than the number of literals
   */
  public ImmutableFormulaList build(List<? extends Literal> lits, List<Integer> coeffs, int rhs) {
    int[] cfs = new int[coeffs.size()];
    for (int i = 0; i < coeffs.size(); i++)
      cfs[i] = coeffs.get(i);
    return this.build(lits.toArray(new Literal[lits.size()]), cfs, rhs);
  }

  /**
   * Builds a pseudo Boolean constraint of the form {@code c_1 * lit_1 + c_2 * lit_2 + ... + c_n * lit_n >= k}.
   * @param lits   the literals {@code lit_1 ... lit_n}
   * @param coeffs the coefficients {@code c_1 ... c_n}
   * @param rhs    the right hand side {@code k} of the constraint
   * @return the CNF encoding of the pseudo Boolean constraint
   * @throws IllegalArgumentException if the right hand side of the cardinality constraint is negative or
   *                                  larger than the number of literals
   */
  public abstract ImmutableFormulaList build(final Literal[] lits, final int[] coeffs, int rhs);
}
