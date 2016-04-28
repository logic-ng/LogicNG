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

import org.logicng.cardinalityconstraints.CCALKTotalizer;
import org.logicng.cardinalityconstraints.CCAMKTotalizer;
import org.logicng.cardinalityconstraints.CCAMOProduct;
import org.logicng.cardinalityconstraints.CCAtLeastK;
import org.logicng.cardinalityconstraints.CCAtMostK;
import org.logicng.cardinalityconstraints.CCAtMostOne;
import org.logicng.cardinalityconstraints.CCEXOProduct;
import org.logicng.cardinalityconstraints.CCExactlyOne;
import org.logicng.collections.ImmutableFormulaList;
import org.logicng.formulas.CType;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.PBConstraint;
import org.logicng.formulas.Variable;

import java.util.LinkedList;
import java.util.List;

/**
 * An encoder for pseudo-Boolean constraints.
 * @version 1.0
 * @since 1.0
 */
public abstract class PBEncoder {

  protected final FormulaFactory f;
  private final CCAtLeastK alk;
  private final CCAtMostK amk;
  private final CCAtMostOne amo;
  private final CCExactlyOne exo;

  /**
   * Constructs a new pseudo-Boolean encoder.
   * @param f the formula factory
   */
  public PBEncoder(final FormulaFactory f) {
    this.f = f;
    this.alk = new CCALKTotalizer(f);
    this.amk = new CCAMKTotalizer(f);
    this.amo = new CCAMOProduct(f);
    this.exo = new CCEXOProduct(f);
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
      return this.buildCC(constraint);
    final Formula normalized = constraint.normalize();
    switch (normalized.type()) {
      case TRUE:
        return new ImmutableFormulaList(FType.AND);
      case FALSE:
        return new ImmutableFormulaList(FType.AND, this.f.falsum());
      case PBC:
        final PBConstraint pbc = (PBConstraint) normalized;
        if (pbc.isCC())
          return this.buildCC(pbc);
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
   * Builds the CNF translation of a given cardinality constraint.
   * @param constraint the cardinality constraint
   * @return the CNF encoding of the pseudo Boolean constraint
   */
  private ImmutableFormulaList buildCC(final PBConstraint constraint) {
    assert constraint.isCC();
    final Variable[] ops = litsAsVars(constraint.operands());
    switch (constraint.comparator()) {
      case LE:
        if (constraint.rhs() == 1)
          return this.amo.build(ops);
        else
          return this.amk.build(ops, constraint.rhs());
      case LT:
        if (constraint.rhs() == 2)
          return this.amo.build(ops);
        else
          return this.amk.build(ops, constraint.rhs() - 1);
      case GE:
        return this.alk.build(ops, constraint.rhs());
      case GT:
        return this.alk.build(ops, constraint.rhs() + 1);
      case EQ:
        if (constraint.rhs() == 1)
          return this.exo.build(ops);
        else {
          Formula le = this.f.cc(CType.LE, constraint.rhs(), ops).normalize();
          Formula ge = this.f.cc(CType.GE, constraint.rhs(), ops).normalize();
          if (le.type() == FType.FALSE || ge.type() == FType.FALSE)
            return new ImmutableFormulaList(FType.AND, this.f.falsum());
          List<Formula> list = new LinkedList<>();
          if (le.type() != FType.TRUE)
            list.addAll(this.build((PBConstraint) le).toList());
          if (ge.type() != FType.TRUE)
            list.addAll(this.build((PBConstraint) ge).toList());
          return new ImmutableFormulaList(FType.AND, list);
        }
      default:
        throw new IllegalArgumentException("Unknown pseudo-Boolean comparator: " + constraint.comparator());
    }
  }

  /**
   * Converts a literal array to a variable array
   * <p>
   * ATTENTION: this only works if because the {@code isCC} method checks, that there are only positive literals.
   * @param lits the literals
   * @return the variables
   */
  private static Variable[] litsAsVars(final Literal[] lits) {
    final Variable[] vars = new Variable[lits.length];
    for (int i = 0; i < vars.length; i++)
      vars[i] = lits[i].variable();
    return vars;
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
