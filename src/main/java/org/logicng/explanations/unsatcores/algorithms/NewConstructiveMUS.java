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

package org.logicng.explanations.unsatcores.algorithms;

import org.logicng.datastructures.Assignment;
import org.logicng.datastructures.Tristate;
import org.logicng.explanations.unsatcores.MUSConfig;
import org.logicng.explanations.unsatcores.UNSATCore;
import org.logicng.formulas.FormulaFactory;
import org.logicng.formulas.Variable;
import org.logicng.propositions.Proposition;
import org.logicng.solvers.MiniSat;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * A new constructive MUS algorithm due to Marques-Silva & Lynce.
 * @version 1.1
 * @since 1.1
 */
public final class NewConstructiveMUS extends MUSAlgorithm {

  @Override
  public UNSATCore computeMUS(List<Proposition> propositions, final FormulaFactory f, final MUSConfig config) {
    final List<Proposition> mus = new ArrayList<>(propositions.size());
    int nrRelaxedProps = propositions.size();
    final MiniSat solver = MiniSat.glucose(f);
    final List<Variable> relexationVars = new ArrayList<>();
    final SortedMap<Variable, Proposition> propMapping = new TreeMap<>();
    for (final Proposition prop : propositions) {
      final Variable relexationVar = f.variable("@MUS_NC_" + relexationVars.size());
      relexationVars.add(relexationVar);
      solver.addWithRelaxation(relexationVar, prop);
      propMapping.put(relexationVar, prop);
    }
    solver.add(f.amo(relexationVars));
    while (nrRelaxedProps > 0) {
      final Tristate result = solver.sat();
      if (result == Tristate.TRUE) {
        final Assignment model = solver.model(relexationVars);
        assert model != null;
        final Variable trueVariable = model.negativeVariables().get(0);
        solver.add(trueVariable.negate());
        mus.add(propMapping.get(trueVariable));
        nrRelaxedProps--;
        relexationVars.remove(trueVariable);
      } else {
        // UNSAT core required...
      }
    }
    return new UNSATCore(mus, true);
  }
}
