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

package org.logicng.solvers.maxsat.algorithms;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.solvers.maxsat.encodings.Encoder;
import org.logicng.util.Pair;

import java.io.PrintStream;
import java.util.HashSet;
import java.util.TreeMap;

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.WeightStrategy;
import static org.logicng.solvers.sat.MiniSatStyleSolver.not;
import static org.logicng.solvers.sat.MiniSatStyleSolver.sign;
import static org.logicng.solvers.sat.MiniSatStyleSolver.var;

/**
 * Incremental WBO solver.
 * @version 1.3
 * @since 1.0
 */
public final class IncWBO extends WBO {

  private final Encoder encoder;
  private final LNGBooleanVector incSoft;
  private final PrintStream output;
  private boolean firstBuild;

  /**
   * Constructs a new solver with default values.
   */
  public IncWBO() {
    this(new MaxSATConfig.Builder().build());
  }

  /**
   * Constructs a new solver with a given configuration.
   * @param config the configuration
   */
  public IncWBO(final MaxSATConfig config) {
    super(config);
    this.solver = null;
    this.verbosity = config.verbosity;
    this.nbCurrentSoft = 0;
    this.weightStrategy = config.weightStrategy;
    this.symmetryStrategy = config.symmetry;
    this.symmetryBreakingLimit = config.limit;
    this.firstBuild = true;
    this.coreMapping = new TreeMap<>();
    this.assumptions = new LNGIntVector();
    this.indexSoftCore = new LNGIntVector();
    this.softMapping = new LNGVector<>();
    this.relaxationMapping = new LNGVector<>();
    this.duplicatedSymmetryClauses = new HashSet<>();
    this.encoder = new Encoder(MaxSATConfig.CardinalityEncoding.TOTALIZER);
    this.encoder.setAMOEncoding(config.amoEncoding);
    this.incSoft = new LNGBooleanVector();
    this.output = config.output;
  }

  @Override
  public MaxSATResult search() {
    nbInitialVariables = nVars();
    if (currentWeight == 1) {
      problemType = ProblemType.UNWEIGHTED;
      weightStrategy = WeightStrategy.NONE;
    }
    if (symmetryStrategy)
      initSymmetry();
    if (problemType == ProblemType.UNWEIGHTED || weightStrategy == WeightStrategy.NONE)
      return this.normalSearch();
    else if (weightStrategy == WeightStrategy.NORMAL || weightStrategy == WeightStrategy.DIVERSIFY)
      return this.weightSearch();
    throw new IllegalArgumentException("Unknown problem type.");
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }

  private void incrementalBuildWeightSolver(final WeightStrategy strategy) {
    assert strategy == WeightStrategy.NORMAL || strategy == WeightStrategy.DIVERSIFY;
    if (this.firstBuild) {
      solver = newSATSolver();
      for (int i = 0; i < nVars(); i++)
        newSATVariable(solver);
      for (int i = 0; i < nHard(); i++)
        solver.addClause(hardClauses.get(i).clause(), null);
      if (symmetryStrategy)
        this.symmetryBreaking();
      this.firstBuild = false;
    }
    LNGIntVector clause;
    nbCurrentSoft = 0;
    for (int i = 0; i < nSoft(); i++) {
      if (softClauses.get(i).weight() >= currentWeight && softClauses.get(i).weight() != 0) {
        nbCurrentSoft++;
        clause = new LNGIntVector(softClauses.get(i).clause());
        for (int j = 0; j < softClauses.get(i).relaxationVars().size(); j++)
          clause.push(softClauses.get(i).relaxationVars().get(j));
        clause.push(softClauses.get(i).assumptionVar());
        solver.addClause(clause, null);
      }
    }
  }

  private void relaxCore(final LNGIntVector conflict, int weightCore) {
    assert conflict.size() > 0;
    assert weightCore > 0;
    LNGIntVector lits = new LNGIntVector();
    for (int i = 0; i < conflict.size(); i++) {
      int indexSoft = coreMapping.get(conflict.get(i));
      if (softClauses.get(indexSoft).weight() == weightCore) {
        LNGIntVector clause = new LNGIntVector(softClauses.get(indexSoft).clause());
        LNGIntVector vars = new LNGIntVector(softClauses.get(indexSoft).relaxationVars());
        final int p = newLiteral(false);
        newSATVariable(solver);
        vars.push(p);
        lits.push(p);
        addSoftClause(weightCore, clause, vars);
        final int l = newLiteral(false);
        newSATVariable(solver);
        softClauses.get(nSoft() - 1).setAssumptionVar(l);
        coreMapping.put(l, nSoft() - 1);
        this.incSoft.set(indexSoft, true);
        this.incSoft.push(false);
        for (int j = 0; j < vars.size(); j++)
          clause.push(vars.get(j));
        clause.push(l);
        solver.addClause(clause, null);
        clause.clear();
        clause.push(softClauses.get(indexSoft).assumptionVar());
        solver.addClause(clause, null);
        if (symmetryStrategy) {
          softMapping.push(new LNGIntVector(softMapping.get(indexSoft)));
          softMapping.get(indexSoft).clear();
          relaxationMapping.push(new LNGIntVector(relaxationMapping.get(indexSoft)));
          relaxationMapping.get(indexSoft).clear();
          symmetryLog(nSoft() - 1);
        }
      } else {
        assert softClauses.get(indexSoft).weight() - weightCore > 0;
        softClauses.get(indexSoft).setWeight(softClauses.get(indexSoft).weight() - weightCore);
        LNGIntVector clause = new LNGIntVector(softClauses.get(indexSoft).clause());
        LNGIntVector vars = new LNGIntVector(softClauses.get(indexSoft).relaxationVars());
        addSoftClause(softClauses.get(indexSoft).weight(), clause, vars);
        if (symmetryStrategy) {
          softMapping.push(new LNGIntVector(softMapping.get(indexSoft)));
          softMapping.get(indexSoft).clear();
          relaxationMapping.push(new LNGIntVector(relaxationMapping.get(indexSoft)));
          relaxationMapping.get(indexSoft).clear();
        }
        this.incSoft.set(indexSoft, true);
        int l = newLiteral(false);
        newSATVariable(solver);
        softClauses.get(nSoft() - 1).setAssumptionVar(l);
        coreMapping.put(l, nSoft() - 1);
        this.incSoft.push(false);
        for (int j = 0; j < vars.size(); j++)
          clause.push(vars.get(j));
        clause.push(l);
        solver.addClause(clause, null);
        clause.clear();
        vars.clear();
        clause = new LNGIntVector(softClauses.get(indexSoft).clause());
        vars = new LNGIntVector(softClauses.get(indexSoft).relaxationVars());
        l = newLiteral(false);
        newSATVariable(solver);
        vars.push(l);
        lits.push(l);
        addSoftClause(weightCore, clause, vars);
        l = newLiteral(false);
        newSATVariable(solver);
        softClauses.get(nSoft() - 1).setAssumptionVar(l);
        coreMapping.put(l, nSoft() - 1);
        this.incSoft.push(false);
        for (int j = 0; j < vars.size(); j++)
          clause.push(vars.get(j));
        clause.push(l);
        solver.addClause(clause, null);
        clause.clear();
        clause.push(softClauses.get(indexSoft).assumptionVar());
        solver.addClause(clause, null);
        if (symmetryStrategy) {
          softMapping.push(new LNGIntVector());
          relaxationMapping.push(new LNGIntVector());
          symmetryLog(nSoft() - 1);
        }
      }
    }
    this.encoder.encodeAMO(solver, lits);
    nbVars = solver.nVars();
    if (symmetryStrategy)
      this.symmetryBreaking();
    sumSizeCores += conflict.size();
  }

  private void symmetryBreaking() {
    if (indexSoftCore.size() != 0 && nbSymmetryClauses < symmetryBreakingLimit) {
      LNGIntVector[] coreIntersection = new LNGIntVector[nbCores];
      LNGIntVector[] coreIntersectionCurrent = new LNGIntVector[nbCores];
      for (int i = 0; i < nbCores; i++) {
        coreIntersection[i] = new LNGIntVector();
        coreIntersectionCurrent[i] = new LNGIntVector();
      }
      LNGIntVector coreList = new LNGIntVector();
      for (int i = 0; i < indexSoftCore.size(); i++) {
        final int p = indexSoftCore.get(i);
        final LNGIntVector addCores = new LNGIntVector();
        for (int j = 0; j < softMapping.get(p).size() - 1; j++) {
          int core = softMapping.get(p).get(j);
          addCores.push(core);
          if (coreIntersection[core].size() == 0)
            coreList.push(core);
          assert j < relaxationMapping.get(p).size();
          assert var(relaxationMapping.get(p).get(j)) > nbInitialVariables;
          coreIntersection[core].push(relaxationMapping.get(p).get(j));
        }
        for (int j = 0; j < addCores.size(); j++) {
          int core = addCores.get(j);
          int b = softMapping.get(p).size() - 1;
          assert b < relaxationMapping.get(p).size();
          assert var(relaxationMapping.get(p).get(b)) > nbInitialVariables;
          coreIntersectionCurrent[core].push(relaxationMapping.get(p).get(b));
        }
        for (int k = 0; k < coreList.size(); k++) {
          for (int m = 0; m < coreIntersection[coreList.get(k)].size(); m++) {
            for (int j = m + 1; j < coreIntersectionCurrent[coreList.get(k)].size(); j++) {
              LNGIntVector clause = new LNGIntVector();
              clause.push(not(coreIntersection[coreList.get(k)].get(m)));
              clause.push(not(coreIntersectionCurrent[coreList.get(k)].get(j)));
              Pair<Integer, Integer> symClause = new Pair<>(var(coreIntersection[coreList.get(k)].get(m)), var(coreIntersectionCurrent[coreList.get(k)].get(j)));
              if (var(coreIntersection[coreList.get(k)].get(m)) > var(coreIntersectionCurrent[coreList.get(k)].get(j)))
                symClause = new Pair<>(var(coreIntersectionCurrent[coreList.get(k)].get(j)), var(coreIntersection[coreList.get(k)].get(m)));
              if (!duplicatedSymmetryClauses.contains(symClause)) {
                duplicatedSymmetryClauses.add(symClause);
                solver.addClause(clause, null);
                nbSymmetryClauses++;
                if (symmetryBreakingLimit == nbSymmetryClauses)
                  break;
              }
            }
            if (symmetryBreakingLimit == nbSymmetryClauses)
              break;
          }
          if (symmetryBreakingLimit == nbSymmetryClauses)
            break;
        }
        if (symmetryBreakingLimit == nbSymmetryClauses)
          break;
      }
    }
    indexSoftCore.clear();
  }

  private MaxSATResult weightSearch() {
    assert weightStrategy == WeightStrategy.NORMAL || weightStrategy == WeightStrategy.DIVERSIFY;
    final Tristate unsatResult = unsatSearch();
    if (unsatResult == UNDEF)
      return MaxSATResult.UNDEF;
    else if (unsatResult == FALSE)
      return MaxSATResult.UNSATISFIABLE;
    initAssumptions(assumptions);
    updateCurrentWeight(weightStrategy);
    this.incrementalBuildWeightSolver(weightStrategy);
    this.incSoft.growTo(nSoft(), false);
    while (true) {
      assumptions.clear();
      for (int i = 0; i < this.incSoft.size(); i++)
        if (!this.incSoft.get(i))
          assumptions.push(not(softClauses.get(i).assumptionVar()));
      final Tristate res = searchSATSolver(solver, satHandler(), assumptions);
      if (res == UNDEF)
        return MaxSATResult.UNDEF;
      else if (res == FALSE) {
        nbCores++;
        assert solver.conflict().size() > 0;
        int coreCost = computeCostCore(solver.conflict());
        lbCost += coreCost;
        if (verbosity != Verbosity.NONE)
          this.output.println(String.format("c LB : %d CS : %d W : %d", lbCost, solver.conflict().size(), coreCost));
        if (!foundLowerBound(lbCost, null))
          return MaxSATResult.UNDEF;
        this.relaxCore(solver.conflict(), coreCost);
        this.incrementalBuildWeightSolver(weightStrategy);
      } else {
        nbSatisfiable++;
        if (nbCurrentSoft == nSoft()) {
          assert this.incComputeCostModel(solver.model()) == lbCost;
          if (lbCost == ubCost && verbosity != Verbosity.NONE)
            this.output.println("c LB = UB");
          if (lbCost < ubCost) {
            ubCost = lbCost;
            saveModel(solver.model());
            if (verbosity != Verbosity.NONE)
              this.output.println("o " + lbCost);
          }
          return MaxSATResult.OPTIMUM;
        } else {
          updateCurrentWeight(weightStrategy);
          int cost = this.incComputeCostModel(solver.model());
          if (cost < ubCost) {
            ubCost = cost;
            saveModel(solver.model());
            if (verbosity != Verbosity.NONE)
              this.output.println("o " + ubCost);
          }
          if (lbCost == ubCost) {
            if (verbosity != Verbosity.NONE)
              this.output.println("c LB = UB");
            return MaxSATResult.OPTIMUM;
          } else if (!foundUpperBound(ubCost, null))
            return MaxSATResult.UNDEF;
          this.incrementalBuildWeightSolver(weightStrategy);
        }
      }
    }
  }

  private int incComputeCostModel(final LNGBooleanVector currentModel) {
    assert currentModel.size() != 0;
    int currentCost = 0;
    for (int i = 0; i < nSoft(); i++) {
      boolean unsatisfied = true;
      for (int j = 0; j < softClauses.get(i).clause().size(); j++) {
        if (this.incSoft.get(i)) {
          unsatisfied = false;
          continue;
        }
        assert var(softClauses.get(i).clause().get(j)) < currentModel.size();
        if ((sign(softClauses.get(i).clause().get(j)) && !currentModel.get(var(softClauses.get(i).clause().get(j)))) ||
                (!sign(softClauses.get(i).clause().get(j)) && currentModel.get(var(softClauses.get(i).clause().get(j))))) {
          unsatisfied = false;
          break;
        }
      }
      if (unsatisfied)
        currentCost += softClauses.get(i).weight();
    }
    return currentCost;
  }

  private MaxSATResult normalSearch() {
    final Tristate unsatResult = unsatSearch();
    if (unsatResult == UNDEF)
      return MaxSATResult.UNDEF;
    else if (unsatResult == FALSE)
      return MaxSATResult.UNSATISFIABLE;
    initAssumptions(assumptions);
    solver = rebuildSolver();
    this.incSoft.growTo(nSoft(), false);
    while (true) {
      assumptions.clear();
      for (int i = 0; i < this.incSoft.size(); i++)
        if (!this.incSoft.get(i))
          assumptions.push(not(softClauses.get(i).assumptionVar()));
      final Tristate res = searchSATSolver(solver, satHandler(), assumptions);
      if (res == UNDEF)
        return MaxSATResult.UNDEF;
      else if (res == FALSE) {
        nbCores++;
        assert solver.conflict().size() > 0;
        int coreCost = computeCostCore(solver.conflict());
        lbCost += coreCost;
        if (verbosity != Verbosity.NONE)
          this.output.println(String.format("c LB : %d CS : %d W : %d", lbCost, solver.conflict().size(), coreCost));
        if (lbCost == ubCost) {
          if (verbosity != Verbosity.NONE)
            this.output.println("c LB = UB");
          return MaxSATResult.OPTIMUM;
        }
        if (!foundLowerBound(lbCost, null))
          return MaxSATResult.UNDEF;
        this.relaxCore(solver.conflict(), coreCost);
      } else {
        nbSatisfiable++;
        ubCost = this.incComputeCostModel(solver.model());
        assert lbCost == ubCost;
        if (verbosity != Verbosity.NONE)
          this.output.println("o " + lbCost);
        saveModel(solver.model());
        return MaxSATResult.OPTIMUM;
      }
    }
  }
}
