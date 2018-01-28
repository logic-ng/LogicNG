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

package org.logicng.transformations.cnf;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.FType;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.formulas.Literal;
import org.logicng.predicates.CNFPredicate;

import java.util.ArrayList;
import java.util.List;

import static org.logicng.formulas.cache.TransformationCacheEntry.TSEITIN;
import static org.logicng.formulas.cache.TransformationCacheEntry.TSEITIN_VARIABLE;

/**
 * Transformation of a formula into CNF due to Tseitin.  Results in this implementation will always be cached.
 * <p>
 * ATTENTION: if you mix formulas from different formula factories this can lead to clashes in the naming of newly
 * introduced variables.
 * @version 1.0
 * @since 1.0
 */
public final class TseitinTransformation implements FormulaTransformation {

  private final int boundaryForFactorization;
  private final CNFPredicate cnfPredicate = new CNFPredicate();
  private final CNFFactorization factorization = new CNFFactorization();

  /**
   * Constructor for a Tseitin transformation.
   * @param boundaryForFactorization the boundary of number of atoms up to which classical factorization is used
   */
  public TseitinTransformation(int boundaryForFactorization) {
    this.boundaryForFactorization = boundaryForFactorization;
  }

  /**
   * Constructor for a Tseitin transformation with a factorization bound of 12.
   */
  public TseitinTransformation() {
    this.boundaryForFactorization = 12;
  }

  @Override
  public Formula apply(final Formula formula, boolean cache) {
    final Formula f = formula.nnf();
    if (f.holds(cnfPredicate))
      return f;
    Formula tseitin = f.transformationCacheEntry(TSEITIN);
    if (tseitin != null) {
      final Assignment topLevel = new Assignment((Literal) f.transformationCacheEntry(TSEITIN_VARIABLE));
      return f.transformationCacheEntry(TSEITIN).restrict(topLevel);
    }
    if (f.numberOfAtoms() < this.boundaryForFactorization)
      tseitin = f.transform(factorization);
    else {
      for (final Formula subformula : f.apply(f.factory().subformulaFunction()))
        computeTseitin(subformula);
      final Assignment topLevel = new Assignment((Literal) f.transformationCacheEntry(TSEITIN_VARIABLE));
      tseitin = f.transformationCacheEntry(TSEITIN).restrict(topLevel);
    }
    if (cache)
      formula.setTransformationCacheEntry(TSEITIN_VARIABLE,
              f.transformationCacheEntry(TSEITIN_VARIABLE));
    return tseitin;
  }

  /**
   * Computes the Tseitin transformation for a given formula and stores it in the formula cache.
   * @param formula the formula
   */
  private void computeTseitin(final Formula formula) {
    if (formula.transformationCacheEntry(TSEITIN) != null)
      return;
    final FormulaFactory f = formula.factory();
    switch (formula.type()) {
      case LITERAL:
        formula.setTransformationCacheEntry(TSEITIN, formula);
        formula.setTransformationCacheEntry(TSEITIN_VARIABLE, formula);
        break;
      case AND:
        Literal tsLiteral = f.newCNFVariable();
        List<Formula> nops = new ArrayList<>();
        List<Formula> operands = new ArrayList<>(formula.numberOfOperands());
        List<Formula> negOperands = new ArrayList<>(formula.numberOfOperands());
        negOperands.add(tsLiteral);
        for (final Formula op : formula) {
          if (op.type() != FType.LITERAL) {
            computeTseitin(op);
            nops.add(op.transformationCacheEntry(TSEITIN));
          }
          operands.add(op.transformationCacheEntry(TSEITIN_VARIABLE));
          negOperands.add(op.transformationCacheEntry(TSEITIN_VARIABLE).negate());
        }
        for (final Formula op : operands)
          nops.add(f.or(tsLiteral.negate(), op));
        nops.add(f.or(negOperands));
        formula.setTransformationCacheEntry(TSEITIN_VARIABLE, tsLiteral);
        formula.setTransformationCacheEntry(TSEITIN, f.and(nops));
        break;
      case OR:
        tsLiteral = f.newCNFVariable();
        nops = new ArrayList<>();
        operands = new ArrayList<>(formula.numberOfOperands());
        negOperands = new ArrayList<>(formula.numberOfOperands());
        operands.add(tsLiteral.negate());
        for (final Formula op : formula) {
          if (op.type() != FType.LITERAL) {
            computeTseitin(op);
            nops.add(op.transformationCacheEntry(TSEITIN));
          }
          operands.add(op.transformationCacheEntry(TSEITIN_VARIABLE));
          negOperands.add(op.transformationCacheEntry(TSEITIN_VARIABLE).negate());
        }
        for (final Formula op : negOperands)
          nops.add(f.or(tsLiteral, op));
        nops.add(f.or(operands));
        formula.setTransformationCacheEntry(TSEITIN_VARIABLE, tsLiteral);
        formula.setTransformationCacheEntry(TSEITIN, f.and(nops));
        break;
      default:
        throw new IllegalArgumentException("Could not process the formula type " + formula.type());
    }
  }

  @Override
  public String toString() {
    return String.format("TseitinTransformation{boundary=%d}", boundaryForFactorization);
  }
}
