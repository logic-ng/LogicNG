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

import org.logicng.collections.LNGIntVector;
import org.logicng.collections.LNGVector;
import org.logicng.datastructures.Tristate;
import org.logicng.solvers.sat.MiniSatStyleSolver;
import org.logicng.util.Pair;

import java.io.PrintStream;
import java.util.HashSet;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import static org.logicng.datastructures.Tristate.FALSE;
import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.WeightStrategy;
import static org.logicng.solvers.sat.MiniSatStyleSolver.not;
import static org.logicng.solvers.sat.MiniSatStyleSolver.var;

/**
 * Weighted Boolean Optimization solver.
 * @version 1.3
 * @since 1.0
 */
public class WBO extends MaxSAT {

  private final PrintStream output;
  protected MiniSatStyleSolver solver;
  protected int nbCurrentSoft;
  protected WeightStrategy weightStrategy;
  protected SortedMap<Integer, Integer> coreMapping;
  protected LNGIntVector assumptions;
  protected boolean symmetryStrategy;
  protected LNGIntVector indexSoftCore;
  protected LNGVector<LNGIntVector> softMapping;
  protected LNGVector<LNGIntVector> relaxationMapping;
  protected Set<Pair<Integer, Integer>> duplicatedSymmetryClauses;
  protected int symmetryBreakingLimit;

  /**
   * Constructs a new solver with default values.
   */
  public WBO() {
    this(new MaxSATConfig.Builder().build());
  }

  /**
   * Constructs a new solver with a given configuration.
   * @param config the configuration
   */
  public WBO(final MaxSATConfig config) {
    super(config);
    this.solver = null;
    verbosity = config.verbosity;
    this.nbCurrentSoft = 0;
    this.weightStrategy = config.weightStrategy;
    this.symmetryStrategy = config.symmetry;
    this.symmetryBreakingLimit = config.limit;
    this.coreMapping = new TreeMap<>();
    this.assumptions = new LNGIntVector();
    this.indexSoftCore = new LNGIntVector();
    this.softMapping = new LNGVector<>();
    this.relaxationMapping = new LNGVector<>();
    this.duplicatedSymmetryClauses = new HashSet<>();
    this.output = config.output;
  }

  @Override
  public MaxSATResult search() {
    nbInitialVariables = nVars();
    if (currentWeight == 1) {
      problemType = ProblemType.UNWEIGHTED;
      this.weightStrategy = WeightStrategy.NONE;
    }
    if (this.symmetryStrategy)
      this.initSymmetry();
    if (problemType == ProblemType.UNWEIGHTED || this.weightStrategy == WeightStrategy.NONE)
      return this.normalSearch();
    else if (this.weightStrategy == WeightStrategy.NORMAL || this.weightStrategy == WeightStrategy.DIVERSIFY)
      return this.weightSearch();
    throw new IllegalArgumentException("Unknown problem type.");
  }


  private MiniSatStyleSolver rebuildWeightSolver(final WeightStrategy strategy) {
    assert strategy == WeightStrategy.NORMAL || strategy == WeightStrategy.DIVERSIFY;
    final MiniSatStyleSolver s = newSATSolver();
    for (int i = 0; i < nVars(); i++)
      newSATVariable(s);
    for (int i = 0; i < nHard(); i++)
      s.addClause(hardClauses.get(i).clause(), null);
    if (this.symmetryStrategy)
      this.symmetryBreaking();
    LNGIntVector clause = new LNGIntVector();
    this.nbCurrentSoft = 0;
    for (int i = 0; i < nSoft(); i++) {
      if (softClauses.get(i).weight() >= currentWeight) {
        this.nbCurrentSoft++;
        clause.clear();
        clause = new LNGIntVector(softClauses.get(i).clause());
        for (int j = 0; j < softClauses.get(i).relaxationVars().size(); j++)
          clause.push(softClauses.get(i).relaxationVars().get(j));
        clause.push(softClauses.get(i).assumptionVar());

        s.addClause(clause, null);
      }
    }
    return s;
  }

  MiniSatStyleSolver rebuildSolver() {
    assert this.weightStrategy == WeightStrategy.NONE;
    final MiniSatStyleSolver s = newSATSolver();
    for (int i = 0; i < nVars(); i++)
      newSATVariable(s);
    for (int i = 0; i < nHard(); i++)
      s.addClause(hardClauses.get(i).clause(), null);
    if (this.symmetryStrategy)
      this.symmetryBreaking();
    LNGIntVector clause;
    for (int i = 0; i < nSoft(); i++) {
      clause = new LNGIntVector(softClauses.get(i).clause());
      for (int j = 0; j < softClauses.get(i).relaxationVars().size(); j++)
        clause.push(softClauses.get(i).relaxationVars().get(j));
      clause.push(softClauses.get(i).assumptionVar());
      s.addClause(clause, null);
    }
    return s;
  }

  private MiniSatStyleSolver rebuildHardSolver() {
    final MiniSatStyleSolver s = newSATSolver();
    for (int i = 0; i < nVars(); i++)
      newSATVariable(s);
    for (int i = 0; i < nHard(); i++)
      s.addClause(hardClauses.get(i).clause(), null);
    return s;
  }

  void updateCurrentWeight(final WeightStrategy strategy) {
    assert strategy == WeightStrategy.NORMAL || strategy == WeightStrategy.DIVERSIFY;
    if (strategy == WeightStrategy.NORMAL)
      currentWeight = this.findNextWeight(currentWeight);
    else if (strategy == WeightStrategy.DIVERSIFY)
      currentWeight = this.findNextWeightDiversity(currentWeight);
  }


  private int findNextWeight(int weight) {
    int nextWeight = 1;
    for (int i = 0; i < nSoft(); i++) {
      if (softClauses.get(i).weight() > nextWeight && softClauses.get(i).weight() < weight)
        nextWeight = softClauses.get(i).weight();
    }
    return nextWeight;
  }

  private int findNextWeightDiversity(int weight) {
    assert this.weightStrategy == WeightStrategy.DIVERSIFY;
    assert nbSatisfiable > 0;
    int nextWeight = weight;
    int nbClauses;
    final SortedSet<Integer> nbWeights = new TreeSet<>();
    double alpha = 1.25;
    boolean findNext = false;
    while (true) {
      if (nbSatisfiable > 1 || findNext)
        nextWeight = this.findNextWeight(nextWeight);
      nbClauses = 0;
      nbWeights.clear();
      for (int i = 0; i < nSoft(); i++) {
        if (softClauses.get(i).weight() >= nextWeight) {
          nbClauses++;
          nbWeights.add(softClauses.get(i).weight());
        }
      }
      if ((double) nbClauses / nbWeights.size() > alpha || nbClauses == nSoft())
        break;
      if (nbSatisfiable == 1 && !findNext)
        findNext = true;
    }
    return nextWeight;
  }

  private void encodeEO(final LNGIntVector lits) {
    assert lits.size() != 0;
    LNGIntVector clause = new LNGIntVector();
    if (lits.size() == 1) {
      clause.push(lits.get(0));
      addHardClause(clause);
    } else {
      LNGIntVector auxVariables = new LNGIntVector();
      for (int i = 0; i < lits.size() - 1; i++)
        auxVariables.push(newLiteral(false));
      for (int i = 0; i < lits.size(); i++) {
        if (i == 0) {
          clause.clear();
          clause.push(lits.get(i));
          clause.push(not(auxVariables.get(i)));
          addHardClause(clause);
          clause.clear();
          clause.push(not(lits.get(i)));
          clause.push(auxVariables.get(i));
          addHardClause(clause);
        } else if (i == lits.size() - 1) {
          clause.clear();
          clause.push(lits.get(i));
          clause.push(auxVariables.get(i - 1));
          addHardClause(clause);
          clause.clear();
          clause.push(not(lits.get(i)));
          clause.push(not(auxVariables.get(i - 1)));
          addHardClause(clause);
        } else {
          clause.clear();
          clause.push(not(auxVariables.get(i - 1)));
          clause.push(auxVariables.get(i));
          addHardClause(clause);
          clause.clear();
          clause.push(lits.get(i));
          clause.push(not(auxVariables.get(i)));
          clause.push(auxVariables.get(i - 1));
          addHardClause(clause);
          clause.clear();
          clause.push(not(lits.get(i)));
          clause.push(auxVariables.get(i));
          addHardClause(clause);
          clause.clear();
          clause.push(not(lits.get(i)));
          clause.push(not(auxVariables.get(i - 1)));
          addHardClause(clause);
        }
      }
    }
  }

  private void relaxCore(final LNGIntVector conflict, int weightCore, final LNGIntVector assumps) {
    assert conflict.size() > 0;
    assert weightCore > 0;
    LNGIntVector lits = new LNGIntVector();
    for (int i = 0; i < conflict.size(); i++) {
      int indexSoft = this.coreMapping.get(conflict.get(i));

      if (softClauses.get(indexSoft).weight() == weightCore) {
        final int p = newLiteral(false);
        softClauses.get(indexSoft).relaxationVars().push(p);
        lits.push(p);
        if (this.symmetryStrategy)
          this.symmetryLog(indexSoft);
      } else {
        assert softClauses.get(indexSoft).weight() - weightCore > 0;
        softClauses.get(indexSoft).setWeight(softClauses.get(indexSoft).weight() - weightCore);
        LNGIntVector clause = new LNGIntVector(softClauses.get(indexSoft).clause());
        LNGIntVector vars = new LNGIntVector(softClauses.get(indexSoft).relaxationVars());
        final int p = newLiteral(false);
        vars.push(p);
        lits.push(p);
        addSoftClause(weightCore, clause, vars);
        final int l = newLiteral(false);
        softClauses.get(nSoft() - 1).setAssumptionVar(l);
        this.coreMapping.put(l, nSoft() - 1);  // Map the new soft clause to its assumption literal.
        assumps.push(not(l)); // Update the assumption vector.
        if (this.symmetryStrategy)
          this.symmetryLog(nSoft() - 1);
      }
    }
    this.encodeEO(lits);
    sumSizeCores += conflict.size();
  }

  int computeCostCore(final LNGIntVector conflict) {
    assert conflict.size() != 0;
    if (problemType == ProblemType.UNWEIGHTED)
      return 1;
    int coreCost = Integer.MAX_VALUE;
    for (int i = 0; i < conflict.size(); i++) {
      int indexSoft = this.coreMapping.get(conflict.get(i));
      if (softClauses.get(indexSoft).weight() < coreCost)
        coreCost = softClauses.get(indexSoft).weight();
    }
    return coreCost;
  }

  void initSymmetry() {
    for (int i = 0; i < nSoft(); i++) {
      this.softMapping.push(new LNGIntVector());
      this.relaxationMapping.push(new LNGIntVector());
    }
  }

  void symmetryLog(int p) {
    if (nbSymmetryClauses < this.symmetryBreakingLimit) {
      while (this.softMapping.size() <= p) {
        this.softMapping.push(new LNGIntVector());
        this.relaxationMapping.push(new LNGIntVector());
      }
      this.softMapping.get(p).push(nbCores);
      this.relaxationMapping.get(p).push(softClauses.get(p).relaxationVars().back());
      if (this.softMapping.get(p).size() > 1)
        this.indexSoftCore.push(p);
    }
  }

  private void symmetryBreaking() {
    if (this.indexSoftCore.size() != 0 && nbSymmetryClauses < this.symmetryBreakingLimit) {
      LNGIntVector[] coreIntersection = new LNGIntVector[nbCores];
      LNGIntVector[] coreIntersectionCurrent = new LNGIntVector[nbCores];
      for (int i = 0; i < nbCores; i++) {
        coreIntersection[i] = new LNGIntVector();
        coreIntersectionCurrent[i] = new LNGIntVector();
      }
      LNGIntVector coreList = new LNGIntVector();
      for (int i = 0; i < this.indexSoftCore.size(); i++) {
        int p = this.indexSoftCore.get(i);
        LNGIntVector addCores = new LNGIntVector();
        for (int j = 0; j < this.softMapping.get(p).size() - 1; j++) {
          int core = this.softMapping.get(p).get(j);
          addCores.push(core);
          if (coreIntersection[core].size() == 0)
            coreList.push(core);
          assert j < this.relaxationMapping.get(p).size();
          assert var(this.relaxationMapping.get(p).get(j)) > nbInitialVariables;
          coreIntersection[core].push(this.relaxationMapping.get(p).get(j));
        }
        for (int j = 0; j < addCores.size(); j++) {
          int core = addCores.get(j);
          int b = this.softMapping.get(p).size() - 1;
          assert b < this.relaxationMapping.get(p).size();
          assert var(this.relaxationMapping.get(p).get(b)) > nbInitialVariables;
          coreIntersectionCurrent[core].push(this.relaxationMapping.get(p).get(b));
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
              if (!this.duplicatedSymmetryClauses.contains(symClause)) {
                this.duplicatedSymmetryClauses.add(symClause);
                addHardClause(clause);
                nbSymmetryClauses++;
                if (this.symmetryBreakingLimit == nbSymmetryClauses)
                  break;
              }
            }
            if (this.symmetryBreakingLimit == nbSymmetryClauses)
              break;
          }
          if (this.symmetryBreakingLimit == nbSymmetryClauses)
            break;
        }
        if (this.symmetryBreakingLimit == nbSymmetryClauses)
          break;
      }
    }
    this.indexSoftCore.clear();
  }

  Tristate unsatSearch() {
    assert this.assumptions.size() == 0;
    this.solver = this.rebuildHardSolver();
    Tristate res = searchSATSolver(this.solver, satHandler(), this.assumptions);
    if (res == FALSE)
      nbCores++;
    else if (res == TRUE) {
      nbSatisfiable++;
      int cost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
      assert cost <= ubCost;
      ubCost = cost;
      saveModel(this.solver.model());
      if (verbosity != Verbosity.NONE)
        this.output.println("o " + ubCost);
    }
    this.solver = null;
    return res;
  }

  private MaxSATResult weightSearch() {
    assert this.weightStrategy == WeightStrategy.NORMAL || this.weightStrategy == WeightStrategy.DIVERSIFY;
    final Tristate unsatResult = this.unsatSearch();
    if (unsatResult == UNDEF)
      return MaxSATResult.UNDEF;
    else if (unsatResult == FALSE)
      return MaxSATResult.UNSATISFIABLE;
    this.initAssumptions(this.assumptions);
    this.updateCurrentWeight(this.weightStrategy);
    this.solver = this.rebuildWeightSolver(this.weightStrategy);
    while (true) {
      final Tristate res = searchSATSolver(this.solver, satHandler(), this.assumptions);
      if (res == UNDEF)
        return MaxSATResult.UNDEF;
      else if (res == FALSE) {
        nbCores++;
        assert this.solver.conflict().size() > 0;
        int coreCost = this.computeCostCore(this.solver.conflict());
        lbCost += coreCost;
        if (verbosity != Verbosity.NONE)
          this.output.println(String.format("c LB : %d CS : %d W : %d", lbCost, this.solver.conflict().size(), coreCost));
        if (!foundLowerBound(lbCost, null))
          return MaxSATResult.UNDEF;
        this.relaxCore(this.solver.conflict(), coreCost, this.assumptions);
        this.solver = this.rebuildWeightSolver(this.weightStrategy);
      } else {
        nbSatisfiable++;
        if (this.nbCurrentSoft == nSoft()) {
          assert computeCostModel(this.solver.model(), Integer.MAX_VALUE) == lbCost;
          if (lbCost == ubCost && verbosity != Verbosity.NONE)
            this.output.println("c LB = UB");
          if (lbCost < ubCost) {
            ubCost = lbCost;
            saveModel(this.solver.model());
            if (verbosity != Verbosity.NONE)
              this.output.println("o " + lbCost);
          }
          return MaxSATResult.OPTIMUM;
        } else {
          this.updateCurrentWeight(this.weightStrategy);
          int cost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
          if (cost < ubCost) {
            ubCost = cost;
            saveModel(this.solver.model());
            if (verbosity != Verbosity.NONE)
              this.output.println("o " + ubCost);
          }
          if (lbCost == ubCost) {
            if (verbosity != Verbosity.NONE)
              this.output.println("c LB = UB");
            return MaxSATResult.OPTIMUM;
          } else if (!foundUpperBound(ubCost, null))
            return MaxSATResult.UNDEF;
          this.solver = this.rebuildWeightSolver(this.weightStrategy);
        }
      }
    }
  }

  private MaxSATResult normalSearch() {
    final Tristate unsatResult = this.unsatSearch();
    if (unsatResult == UNDEF)
      return MaxSATResult.UNDEF;
    else if (unsatResult == FALSE)
      return MaxSATResult.UNSATISFIABLE;
    this.initAssumptions(this.assumptions);
    this.solver = this.rebuildSolver();
    while (true) {
      final Tristate res = searchSATSolver(this.solver, satHandler(), this.assumptions);
      if (res == UNDEF)
        return MaxSATResult.UNDEF;
      else if (res == FALSE) {
        nbCores++;
        assert this.solver.conflict().size() > 0;
        int coreCost = this.computeCostCore(this.solver.conflict());
        lbCost += coreCost;
        if (verbosity != Verbosity.NONE)
          this.output.println(String.format("c LB : %d CS : %d W : %d", lbCost, this.solver.conflict().size(), coreCost));
        if (lbCost == ubCost) {
          if (verbosity != Verbosity.NONE)
            this.output.println("c LB = UB");
          return MaxSATResult.OPTIMUM;
        } else if (!foundLowerBound(lbCost, null))
          return MaxSATResult.UNDEF;
        this.relaxCore(this.solver.conflict(), coreCost, this.assumptions);
        this.solver = this.rebuildSolver();
      } else {
        nbSatisfiable++;
        ubCost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
        assert lbCost == ubCost;
        if (verbosity != Verbosity.NONE)
          this.output.println("o " + lbCost);
        saveModel(this.solver.model());
        return MaxSATResult.OPTIMUM;
      }
    }
  }

  void initAssumptions(final LNGIntVector assumps) {
    for (int i = 0; i < nbSoft; i++) {
      final int l = newLiteral(false);
      softClauses.get(i).setAssumptionVar(l);
      this.coreMapping.put(l, i);
      assumps.push(not(l));
    }
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
