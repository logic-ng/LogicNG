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

package org.logicng.cardinalityconstraints;

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.util.ArrayList;
import java.util.List;

/**
 * The result of a cardinality constraint encoding.
 * @version 1.1
 * @since 1.1
 */
final class CCResult {
  final FormulaFactory f;
  private List<Formula> result;
  private final MiniSatStyleSolver miniSat;

  /**
   * Constructs a new CC encoding algorithm.
   * @param f the formula factory
   */
  private CCResult(final FormulaFactory f, final MiniSatStyleSolver miniSat) {
    this.f = f;
    this.miniSat = miniSat;
  }

  /**
   * Constructs a new result which stores the result in a formula.
   * @param f the formula factory
   * @return the result
   */
  static CCResult resultForFormula(final FormulaFactory f) {
    return new CCResult(f, null);
  }

  /**
   * Constructs a new result which adds the result directly to a given MiniSat solver.
   * @param f       the formula factory
   * @param miniSat the solver
   * @return the result
   */
  static CCResult resultForMiniSat(final FormulaFactory f, final MiniSatStyleSolver miniSat) {
    return new CCResult(f, miniSat);
  }

  /**
   * Adds a clause to the result
   * @param literals the literals of the clause
   */
  void addClause(final Literal... literals) {
    if (miniSat == null)
      result.add(this.f.clause(literals));
    else {
      final LNGIntVector clauseVec = new LNGIntVector(literals.length);
      for (Literal lit : literals) {
        int index = this.miniSat.idxForName(lit.name());
        assert index != -1;
        int litNum = lit.phase() ? index * 2 : (index * 2) ^ 1;
        clauseVec.push(litNum);
      }
      miniSat.addClause(clauseVec);
    }
  }

  /**
   * Adds a clause to the result
   * @param literals the literals of the clause
   */
  void addClause(final LNGVector<Literal> literals) {
    if (miniSat == null)
      result.add(vec2clause(literals));
    else {
      final LNGIntVector clauseVec = new LNGIntVector(literals.size());
      for (Literal lit : literals) {
        int index = this.miniSat.idxForName(lit.name());
        assert index != -1;
        int litNum = lit.phase() ? index * 2 : (index * 2) ^ 1;
        clauseVec.push(litNum);
      }
      miniSat.addClause(clauseVec);
    }
  }

  /**
   * Returns a clause for a vector of literals.
   * @param literals the literals
   * @return the clause
   */
  private Formula vec2clause(final LNGVector<Literal> literals) {
    final List<Literal> lits = new ArrayList<>(literals.size());
    for (final Literal l : literals)
      lits.add(l);
    return this.f.clause(lits);
  }

  /**
   * Returns a new auxiliary variable.
   * @return a new auxiliary variable
   */
  Variable newVariable() {
    if (miniSat == null)
      return this.f.newCCVariable();
    else {
      final int index = this.miniSat.newVar(true, true);
      final String name = FormulaFactory.CC_PREFIX + "MINISAT_" + index;
      this.miniSat.addName(name, index);
      return new CCAuxiliaryVariable(name);
    }
  }

  /**
   * Resets the result.
   */
  void reset() {
    this.result = new ArrayList<>();
  }

  /**
   * Returns the result of this algorithm.
   * @return the result of this algorithm
   */
  List<Formula> result() {
    return this.result;
  }
}
