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
//  Copyright 2015 Christoph Zengler                                     //
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
import org.logicng.formulas.Not;
import org.logicng.predicates.CNFPredicate;

import java.util.ArrayList;
import java.util.List;

import static org.logicng.formulas.cache.TransformationCacheEntry.PLAISTED_GREENBAUM_NEG;
import static org.logicng.formulas.cache.TransformationCacheEntry.PLAISTED_GREENBAUM_POS;
import static org.logicng.formulas.cache.TransformationCacheEntry.PLAISTED_GREENBAUM_VARIABLE;

/**
 * Transformation of a formula into CNF due to Plaisted &amp; Greenbaum.  Results in this implementation will be always
 * cached.
 * ATTENTION: if you mix formulas from different formula factories this can lead to clashes in the naming of newly
 * introduced variables.
 * @author Christoph Zengler
 * @version 1.0
 * @since 1.0
 */
public final class PlaistedGreenbaumTransformation implements FormulaTransformation {

  private final int boundaryForFactorization;
  private final boolean nnf;
  private final CNFPredicate cnfPredicate = new CNFPredicate();

  /**
   * Constructor.
   * @param boundaryForFactorization the boundary of number of atoms up to which classical factorization is used
   * @param nnf                      indicates whether the formula should be transformed to NNF before converting it
   *                                 to CNF or not (transforming the formula to NNF usually reduces the number of
   *                                 introduced variables and transforming time)
   */
  public PlaistedGreenbaumTransformation(int boundaryForFactorization, boolean nnf) {
    this.boundaryForFactorization = boundaryForFactorization;
    this.nnf = nnf;
  }

  /**
   * Constructor.
   * @param nnf indicates whether the formula should be transformed to NNF before converting it to CNF or not
   *            (transforming the formula to NNF usually reduces the number of introduced variables and transforming
   *            time)
   */
  public PlaistedGreenbaumTransformation(boolean nnf) {
    this(20, nnf);
  }

  /**
   * Constructor.
   */
  public PlaistedGreenbaumTransformation() {
    this(20, true);
  }

  @Override
  public Formula apply(final Formula formula, boolean cache) {
    final Formula f = this.nnf ? formula.nnf() : formula;
    if (f.holds(this.cnfPredicate))
      return f;
    Formula pg;
    if (f.numberOfAtoms() < this.boundaryForFactorization)
      pg = f.cnf();
    else {
      pg = this.computeTransformation(true, f, null);
      final Assignment topLevel = new Assignment((Literal) f.getTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE));
      pg = pg.restrict(topLevel);
    }
    if (this.nnf)
      formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE,
              f.getTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE));
    return pg;
  }

  private Formula computeTransformation(boolean polarity, final Formula formula, final Literal fixedPGVar) {
    final FormulaFactory f = formula.factory();
    switch (formula.type()) {
      case LITERAL:
        return f.verum();
      case NOT:
        final Formula first = polarity
                ? this.computePosPolarity(formula, fixedPGVar)
                : this.computeNegPolarity(formula, fixedPGVar);
        final Formula second = this.computeTransformation(!polarity, ((Not) formula).operand(), null);
        return f.and(first, second);
      case OR:
      case AND:
        final List<Formula> nops = new ArrayList<>();
        nops.add(polarity
                ? this.computePosPolarity(formula, fixedPGVar)
                : this.computeNegPolarity(formula, fixedPGVar));
        for (final Formula op : formula)
          nops.add(this.computeTransformation(polarity, op, null));
        return f.and(nops);
      case IMPL:
      case EQUIV:
      case PBC:
        final Formula nnfFormula = formula.nnf();
        final Formula fixedVar = formula.getTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE);
        final Formula nnfPG = this.computeTransformation(polarity, nnfFormula, fixedVar == null ? null : (Literal) fixedVar);
        formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE,
                nnfFormula.getTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE));
        return nnfPG;
      default:
        throw new IllegalArgumentException("Could not process the formula type " + formula.type());
    }
  }

  private Formula computePosPolarity(final Formula formula, final Literal fixedPGVar) {
    Formula result = formula.getTransformationCacheEntry(PLAISTED_GREENBAUM_POS);
    if (result != null)
      return result;
    final FormulaFactory f = formula.factory();
    final Formula pgVar = fixedPGVar != null ? fixedPGVar : pgVariable(formula);
    switch (formula.type()) {
      case NOT:
        result = f.or(pgVar.negate(), pgVariable(((Not) formula).operand()).negate());
        formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_POS, result);
        return result;
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

  private Formula computeNegPolarity(final Formula formula, final Literal fixedPGVar) {
    Formula result = formula.getTransformationCacheEntry(PLAISTED_GREENBAUM_NEG);
    if (result != null)
      return result;
    final FormulaFactory f = formula.factory();
    final Formula pgVar = fixedPGVar != null ? fixedPGVar : pgVariable(formula);
    switch (formula.type()) {
      case NOT:
        result = f.or(pgVar, pgVariable(((Not) formula).operand()));
        formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_NEG, result);
        return result;
      case AND:
        List<Formula> nops = new ArrayList<>();
        nops.add(pgVar);
        for (final Formula op : formula)
          nops.add(pgVariable(op).negate());
        result = f.or(nops);
        formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_NEG, result);
        return result;
      case OR:
        nops = new ArrayList<>();
        for (final Formula op : formula)
          nops.add(f.or(pgVar, pgVariable(op).negate()));
        result = f.and(nops);
        formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_NEG, result);
        return result;
      default:
        throw new IllegalArgumentException("not yet implemented");
    }
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
    Formula var = formula.getTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE);
    if (var == null) {
      var = formula.factory().newCNFLiteral();
      formula.setTransformationCacheEntry(PLAISTED_GREENBAUM_VARIABLE, var);
    }
    return var;
  }

  @Override
  public String toString() {
    return String.format("PlaistedGreenbaumTransformation{boundary=%d, nnf=%s}", this.boundaryForFactorization, this.nnf);
  }
}
