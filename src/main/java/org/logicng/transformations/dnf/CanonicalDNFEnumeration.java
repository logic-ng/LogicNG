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

package org.logicng.transformations.dnf;

import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.FormulaTransformation;
import org.logicng.solvers.MiniSat;
import org.logicng.solvers.SATSolver;

import java.util.LinkedList;
import java.util.List;

/**
 * Canonical DNF generation via enumeration of models by a SAT solver.
 * @version 1.0
 * @since 1.0
 */
public final class CanonicalDNFEnumeration implements FormulaTransformation {

  @Override
  public Formula apply(final Formula formula, boolean cache) {
    final FormulaFactory f = formula.factory();
    final SATSolver solver = MiniSat.miniSat(f);
    solver.add(formula);
    final List<Assignment> enumeration = solver.enumerateAllModels();
    if (enumeration.isEmpty())
      return f.falsum();
    final List<Formula> ops = new LinkedList<>();
    for (final Assignment a : enumeration)
      ops.add(a.formula(f));
    return f.or(ops);
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
