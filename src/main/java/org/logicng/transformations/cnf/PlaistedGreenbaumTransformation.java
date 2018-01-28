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

import static org.logicng.formulas.cache.TransformationCacheEntry.PLAISTED_GREENBAUM_POS;
import static org.logicng.formulas.cache.TransformationCacheEntry.PLAISTED_GREENBAUM_VARIABLE;

/**
 * Transformation of a formula into CNF due to Plaisted &amp; Greenbaum.  Results in this implementation will always be
 * cached.
 * <p>
 * ATTENTION: if you mix formulas from different formula factories this can lead to clashes in the naming of newly
 * introduced variables.
 * @version 1.1
 * @since 1.0
 */
public final class PlaistedGreenbaumTransformation implements FormulaTransformation {

  private final int boundaryForFactorization;
  private final CNFPredicate cnfPredicate = new CNFPredicate();
  private final CNFFactorization factorization = new CNFFactorization();

  /**
   * Constructor for a Plaisted &amp; Greenbaum transformation.
   * @param boundaryForFactorization the boundary of number of atoms up to which classical factorization is used
   */
  public PlaistedGreenbaumTransformation(int boundaryForFactorization) {
    this.boundaryForFactorization = boundaryForFactorization;
  }

  /**
   * Constructor for a Plaisted &amp; Greenbaum transformation with conversion to nnf and a factorization
   * bound of 12.
   */
  public PlaistedGreenbaumTransformation() {
    this(12);
  }

  /**
   * Returns the auxiliary variable for a given formula.  Either the formula is already a variable, has already an
   * auxiliary variable or a new one is
   * generated.
   * @param formula the formula
   * @return the old or new auxiliary variable
   */
  private static Formula pgVariable(final Formula formula) {
    if (formula.type() == FType.LITERAL)
      return formula;
    Formula var = formula.transformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE);
    if (var == null) {
      var = formula.factory().newCNFVariable();
      formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE, var);
    }
    return var;
  }

  @Override
  public Formula apply(final Formula formula, boolean cache) {
    final Formula f = formula.nnf();
    if (f.holds(this.cnfPredicate))
      return f;
    Formula pg;
    if (f.numberOfAtoms() < this.boundaryForFactorization)
      pg = f.transform(factorization);
    else {
      pg = this.computeTransformation(f, null);
      final Assignment topLevel = new Assignment((Literal) f.transformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE));
      pg = pg.restrict(topLevel);
    }
    if (cache)
      formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE,
              f.transformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE));
    return pg;
  }

  private Formula computeTransformation(final Formula formula, final Literal fixedPGVar) {
    final FormulaFactory f = formula.factory();
    switch (formula.type()) {
      case LITERAL:
        return f.verum();
      case OR:
      case AND:
        final List<Formula> nops = new ArrayList<>();
        nops.add(this.computePosPolarity(formula, fixedPGVar));
        for (final Formula op : formula)
          nops.add(this.computeTransformation(op, null));
        return f.and(nops);
      default:
        throw new IllegalArgumentException("Could not process the formula type " + formula.type());
    }
  }

  private Formula computePosPolarity(final Formula formula, final Literal fixedPGVar) {
    Formula result = formula.transformationCacheEntry(PLAISTED_GREENBAUM_POS);
    if (result != null)
      return result;
    final FormulaFactory f = formula.factory();
    final Formula pgVar = fixedPGVar != null ? fixedPGVar : pgVariable(formula);
    switch (formula.type()) {
      case AND:
        List<Formula> nops = new ArrayList<>();
        for (final Formula op : formula)
          nops.add(f.or(pgVar.negate(), pgVariable(op)));
        result = f.and(nops);
        formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_POS, result);
        return result;
      case OR:
        nops = new ArrayList<>();
        nops.add(pgVar.negate());
        for (final Formula op : formula)
          nops.add(pgVariable(op));
        result = f.or(nops);
        formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_POS, result);
        return result;
      default:
        throw new IllegalArgumentException("not yet implemented");
    }
  }

  @Override
  public String toString() {
    return String.format("PlaistedGreenbaumTransformation{boundary=%d}", this.boundaryForFactorization);
  }
}
