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
import org.logicng.solvers.sat.MiniSatStyleSolver;

import java.io.PrintStream;
import java.util.SortedMap;
import java.util.TreeMap;

import static org.logicng.datastructures.Tristate.TRUE;
import static org.logicng.datastructures.Tristate.UNDEF;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.CardinalityEncoding;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.IncrementalStrategy;
import static org.logicng.solvers.maxsat.algorithms.MaxSATConfig.Verbosity;
import static org.logicng.solvers.sat.MiniSatStyleSolver.not;

/**
 * The weighted MSU3 algorithm.
 * @version 1.3
 * @since 1.0
 */
public final class WMSU3 extends MaxSAT {

  final boolean bmoStrategy;
  final private Encoder encoder;
  final private IncrementalStrategy incrementalStrategy;
  final private LNGIntVector assumptions;
  final private LNGIntVector objFunction;
  final private LNGIntVector coeffs;
  final private SortedMap<Integer, Integer> coreMapping;
  final private LNGBooleanVector activeSoft;
  final private PrintStream output;
  boolean isBmo;
  private MiniSatStyleSolver solver;

  /**
   * Constructs a new solver with default values.
   */
  public WMSU3() {
    this(new MaxSATConfig.Builder().incremental(IncrementalStrategy.ITERATIVE).build());
  }

  /**
   * Constructs a new solver with a given configuration.
   * @param config the configuration
   */
  public WMSU3(final MaxSATConfig config) {
    super(config);
    this.solver = null;
    this.verbosity = config.verbosity;
    this.incrementalStrategy = config.incrementalStrategy;
    this.encoder = new Encoder(config.cardinalityEncoding);
    this.encoder.setPBEncoding(config.pbEncoding);
    this.bmoStrategy = config.bmo;
    this.isBmo = false;
    this.assumptions = new LNGIntVector();
    this.objFunction = new LNGIntVector();
    this.coeffs = new LNGIntVector();
    this.coreMapping = new TreeMap<>();
    this.activeSoft = new LNGBooleanVector();
    this.output = config.output;
  }

  private static boolean subsetSum(final LNGIntVector set, int sum) {
    int n = set.size();
    boolean[][] subset = new boolean[sum + 1][];
    for (int i = 0; i <= sum; i++)
      subset[i] = new boolean[n + 1];
    for (int i = 0; i <= n; i++)
      subset[0][i] = true;
    for (int i = 1; i <= sum; i++)
      subset[i][0] = false;
    for (int i = 1; i <= sum; i++) {
      for (int j = 1; j <= n; j++) {
        subset[i][j] = subset[i][j - 1];
        if (i >= set.get(j - 1))
          subset[i][j] = subset[i][j] || subset[i - set.get(j - 1)][j - 1];
      }
    }
    return subset[sum][n];
  }

  @Override
  public MaxSATResult search() {
    if (problemType == ProblemType.UNWEIGHTED)
      throw new IllegalStateException("Error: Currently algorithm WMSU3 does not support unweighted MaxSAT instances.");
    if (this.bmoStrategy)
      this.isBmo = isBMO(true);
    if (!this.isBmo)
      currentWeight = 1;
    switch (this.incrementalStrategy) {
      case NONE:
        return this.none();
      case ITERATIVE:
        if (this.isBmo) {
          if (this.encoder.cardEncoding() != CardinalityEncoding.TOTALIZER)
            throw new IllegalStateException("Error: Currently iterative encoding in WMSU3 only supports the Totalizer encoding.");
          return this.iterativeBmo();
        } else
          return this.iterative();
      default:
        throw new IllegalArgumentException("Unknown incremental strategy: " + this.incrementalStrategy);
    }
  }

  private MaxSATResult iterative() {
    nbInitialVariables = nVars();
    Tristate res;
    this.initRelaxation();
    this.solver = this.rebuildSolver();
    this.encoder.setIncremental(IncrementalStrategy.ITERATIVE);
    this.activeSoft.growTo(nSoft(), false);
    for (int i = 0; i < nSoft(); i++)
      this.coreMapping.put(softClauses.get(i).assumptionVar(), i);
    this.assumptions.clear();
    LNGIntVector fullObjFunction = new LNGIntVector();
    LNGIntVector fullCoeffsFunction = new LNGIntVector();
    while (true) {
      res = searchSATSolver(this.solver, satHandler(), this.assumptions);
      if (res == UNDEF)
        return MaxSATResult.UNDEF;
      else if (res == TRUE) {
        nbSatisfiable++;
        int newCost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
        if (newCost < ubCost || nbSatisfiable == 1) {
          saveModel(this.solver.model());
          if (verbosity != Verbosity.NONE)
            this.output.println("o " + newCost);
          ubCost = newCost;
        }
        if (ubCost == 0 || lbCost == ubCost || (currentWeight == 1 && nbSatisfiable > 1)) {
          assert lbCost == ubCost;
          assert nbSatisfiable > 0;
          return MaxSATResult.OPTIMUM;
        } else if (!foundUpperBound(ubCost, null))
          return MaxSATResult.UNDEF;
        for (int i = 0; i < nSoft(); i++)
          if (softClauses.get(i).weight() >= currentWeight && !this.activeSoft.get(i))
            this.assumptions.push(not(softClauses.get(i).assumptionVar()));
      } else {
        nbCores++;
        if (nbSatisfiable == 0)
          return MaxSATResult.UNSATISFIABLE;
        else if (lbCost == ubCost) {
          assert nbSatisfiable > 0;
          if (verbosity != Verbosity.NONE)
            this.output.println("c LB = UB");
          return MaxSATResult.OPTIMUM;
        } else if (!foundLowerBound(lbCost, null))
          return MaxSATResult.UNDEF;
        sumSizeCores += this.solver.conflict().size();
        this.objFunction.clear();
        this.coeffs.clear();
        this.assumptions.clear();
        for (int i = 0; i < this.solver.conflict().size(); i++) {
          if (!this.coreMapping.containsKey(this.solver.conflict().get(i)))
            continue;
          int indexSoft = this.coreMapping.get(this.solver.conflict().get(i));
          if (!this.activeSoft.get(indexSoft)) {
            this.activeSoft.set(indexSoft, true);
            this.objFunction.push(softClauses.get(indexSoft).relaxationVars().get(0));
            this.coeffs.push(softClauses.get(indexSoft).weight());
          }
        }
        for (int i = 0; i < nSoft(); i++)
          if (!this.activeSoft.get(i) && softClauses.get(i).weight() >= currentWeight)
            this.assumptions.push(not(softClauses.get(i).assumptionVar()));
        for (int i = 0; i < this.coeffs.size(); i++) {
          fullCoeffsFunction.push(this.coeffs.get(i));
          fullObjFunction.push(this.objFunction.get(i));
        }
        if (verbosity != Verbosity.NONE)
          this.output.println(String.format("c Relaxed soft clauses %d / %d", fullCoeffsFunction.size(), nSoft()));
        lbCost++;
        while (!subsetSum(fullCoeffsFunction, lbCost))
          lbCost++;
        if (verbosity != Verbosity.NONE)
          this.output.println("c LB : " + lbCost);
        if (!this.encoder.hasPBEncoding())
          this.encoder.incEncodePB(this.solver, this.objFunction, this.coeffs, lbCost, this.assumptions, nSoft());
        else {
          this.encoder.incUpdatePB(this.solver, this.objFunction, this.coeffs, lbCost);
          this.encoder.incUpdatePBAssumptions(this.assumptions);
        }
      }
    }
  }

  private MaxSATResult none() {
    nbInitialVariables = nVars();
    Tristate res;
    this.initRelaxation();
    this.solver = this.rebuildSolver();
    this.encoder.setIncremental(IncrementalStrategy.NONE);
    this.activeSoft.growTo(nSoft(), false);
    for (int i = 0; i < nSoft(); i++)
      this.coreMapping.put(softClauses.get(i).assumptionVar(), i);
    this.assumptions.clear();
    while (true) {
      res = searchSATSolver(this.solver, satHandler(), this.assumptions);
      if (res == UNDEF)
        return MaxSATResult.UNDEF;
      else if (res == TRUE) {
        nbSatisfiable++;
        int newCost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
        if (newCost < ubCost || nbSatisfiable == 1) {
          saveModel(this.solver.model());
          if (verbosity != Verbosity.NONE)
            this.output.println("o " + newCost);
          ubCost = newCost;
        }
        if (ubCost == 0 || lbCost == ubCost || (currentWeight == 1 && nbSatisfiable > 1)) {
          assert nbSatisfiable > 0;
          return MaxSATResult.OPTIMUM;
        } else if (!foundUpperBound(ubCost, null))
          return MaxSATResult.UNDEF;
        for (int i = 0; i < nSoft(); i++)
          if (softClauses.get(i).weight() >= currentWeight && !this.activeSoft.get(i))
            this.assumptions.push(not(softClauses.get(i).assumptionVar()));
      } else {
        nbCores++;
        if (nbSatisfiable == 0)
          return MaxSATResult.UNSATISFIABLE;
        else if (lbCost == ubCost) {
          assert nbSatisfiable > 0;
          if (verbosity != Verbosity.NONE)
            this.output.println("c LB = UB");
          return MaxSATResult.OPTIMUM;
        } else if (!foundLowerBound(lbCost, null))
          return MaxSATResult.UNDEF;
        sumSizeCores += this.solver.conflict().size();
        for (int i = 0; i < this.solver.conflict().size(); i++) {
          int indexSoft = this.coreMapping.get(this.solver.conflict().get(i));
          assert !this.activeSoft.get(indexSoft);
          this.activeSoft.set(indexSoft, true);
        }
        this.objFunction.clear();
        this.coeffs.clear();
        this.assumptions.clear();
        for (int i = 0; i < nSoft(); i++) {
          if (this.activeSoft.get(i)) {
            this.objFunction.push(softClauses.get(i).relaxationVars().get(0));
            this.coeffs.push(softClauses.get(i).weight());
          } else if (softClauses.get(i).weight() >= currentWeight)
            this.assumptions.push(not(softClauses.get(i).assumptionVar()));
        }
        if (verbosity != Verbosity.NONE)
          this.output.println(String.format("c Relaxed soft clauses %d / %d", this.objFunction.size(), nSoft()));
        this.solver = this.rebuildSolver();
        lbCost++;
        while (!subsetSum(this.coeffs, lbCost))
          lbCost++;
        if (verbosity != Verbosity.NONE)
          this.output.println("c LB : " + lbCost);
        this.encoder.encodePB(this.solver, this.objFunction, this.coeffs, lbCost);
      }
    }
  }

  private MaxSATResult iterativeBmo() {
    assert this.isBmo;
    nbInitialVariables = nVars();
    Tristate res;
    this.initRelaxation();
    this.solver = this.rebuildSolver();
    this.encoder.setIncremental(IncrementalStrategy.ITERATIVE);
    LNGIntVector joinObjFunction = new LNGIntVector();
    LNGIntVector encodingAssumptions = new LNGIntVector();
    LNGIntVector joinCoeffs = new LNGIntVector();
    this.activeSoft.growTo(nSoft(), false);
    for (int i = 0; i < nSoft(); i++)
      this.coreMapping.put(softClauses.get(i).assumptionVar(), i);
    int minWeight = 0;
    int posWeight = 0;
    int localCost = 0;
    LNGVector<LNGIntVector> functions = new LNGVector<>();
    LNGIntVector weights = new LNGIntVector();
    LNGVector<Encoder> bmoEncodings = new LNGVector<>();
    LNGBooleanVector firstEncoding = new LNGBooleanVector();
    functions.push(new LNGIntVector());
    weights.push(0);
    assert this.objFunction.size() == 0;
    Encoder e = new Encoder(CardinalityEncoding.TOTALIZER);
    e.setIncremental(IncrementalStrategy.ITERATIVE);
    bmoEncodings.push(e);
    firstEncoding.push(true);
    while (true) {
      res = searchSATSolver(this.solver, satHandler(), this.assumptions);
      if (res == UNDEF)
        return MaxSATResult.UNDEF;
      else if (res == TRUE) {
        nbSatisfiable++;
        int newCost = computeCostModel(this.solver.model(), Integer.MAX_VALUE);
        if (newCost < ubCost || nbSatisfiable == 1) {
          saveModel(this.solver.model());
          if (verbosity != Verbosity.NONE)
            this.output.println("o " + newCost);
          ubCost = newCost;
        }
        if (nbSatisfiable == 1) {
          if (ubCost == 0)
            return MaxSATResult.OPTIMUM;
          else if (!foundUpperBound(ubCost, null))
            return MaxSATResult.UNDEF;
          assert orderWeights.size() > 0;
          assert orderWeights.get(0) > 1;
          minWeight = orderWeights.get(orderWeights.size() - 1);
          currentWeight = orderWeights.get(0);
          for (int i = 0; i < nSoft(); i++)
            if (softClauses.get(i).weight() >= currentWeight)
              this.assumptions.push(not(softClauses.get(i).assumptionVar()));
        } else {
          if (currentWeight == 1 || currentWeight == minWeight)
            return MaxSATResult.OPTIMUM;
          else {
            if (!foundUpperBound(ubCost, null))
              return MaxSATResult.UNDEF;
            this.assumptions.clear();
            int previousWeight = currentWeight;
            posWeight++;
            assert posWeight < orderWeights.size();
            currentWeight = orderWeights.get(posWeight);
            if (this.objFunction.size() > 0)
              functions.set(functions.size() - 1, new LNGIntVector(this.objFunction));
            functions.push(new LNGIntVector());
            weights.push(0);
            localCost = 0;
            e = new Encoder(CardinalityEncoding.TOTALIZER);
            e.setIncremental(IncrementalStrategy.ITERATIVE);
            bmoEncodings.push(e);
            firstEncoding.push(true);
            for (int i = 0; i < encodingAssumptions.size(); i++)
              this.solver.addClause(encodingAssumptions.get(i), null);
            encodingAssumptions.clear();
            for (int i = 0; i < nSoft(); i++) {
              if (!this.activeSoft.get(i) && previousWeight == softClauses.get(i).weight())
                this.solver.addClause(not(softClauses.get(i).assumptionVar()), null);
              if (currentWeight == softClauses.get(i).weight())
                this.assumptions.push(not(softClauses.get(i).assumptionVar()));
              if (this.activeSoft.get(i)) {
                assert softClauses.get(i).weight() == previousWeight;
                this.activeSoft.set(i, false);
              }
            }
          }
        }
      } else {
        localCost++;
        lbCost += currentWeight;
        nbCores++;
        if (verbosity != Verbosity.NONE)
          this.output.println("c LB : " + lbCost);
        if (nbSatisfiable == 0)
          return MaxSATResult.UNSATISFIABLE;
        else if (lbCost == ubCost) {
          assert nbSatisfiable > 0;
          if (verbosity != Verbosity.NONE)
            this.output.println("c LB = UB");
          return MaxSATResult.OPTIMUM;
        } else if (!foundLowerBound(lbCost, null))
          return MaxSATResult.UNDEF;
        sumSizeCores += this.solver.conflict().size();
        joinObjFunction.clear();
        joinCoeffs.clear();
        for (int i = 0; i < this.solver.conflict().size(); i++) {
          if (this.coreMapping.containsKey(this.solver.conflict().get(i))) {
            if (this.activeSoft.get(this.coreMapping.get(this.solver.conflict().get(i))))
              continue;
            assert softClauses.get(this.coreMapping.get(this.solver.conflict().get(i))).weight() == currentWeight;
            this.activeSoft.set(this.coreMapping.get(this.solver.conflict().get(i)), true);
            joinObjFunction.push(softClauses.get(this.coreMapping.get(this.solver.conflict().get(i))).relaxationVars().get(0));
            joinCoeffs.push(softClauses.get(this.coreMapping.get(this.solver.conflict().get(i))).weight());
          }
        }
        this.objFunction.clear();
        this.coeffs.clear();
        this.assumptions.clear();
        for (int i = 0; i < nSoft(); i++) {
          if (this.activeSoft.get(i)) {
            assert softClauses.get(i).weight() == currentWeight;
            this.objFunction.push(softClauses.get(i).relaxationVars().get(0));
            this.coeffs.push(softClauses.get(i).weight());
          } else if (currentWeight == softClauses.get(i).weight())
            this.assumptions.push(not(softClauses.get(i).assumptionVar()));
        }
        if (verbosity != Verbosity.NONE)
          this.output.println(String.format("c Relaxed soft clauses %d / %d", this.objFunction.size(), nSoft()));
        assert posWeight < functions.size();
        functions.set(posWeight, new LNGIntVector(this.objFunction));
        weights.set(posWeight, localCost);
        if (firstEncoding.get(posWeight)) {
          if (weights.get(posWeight) != this.objFunction.size()) {
            bmoEncodings.get(posWeight).buildCardinality(this.solver, this.objFunction, weights.get(posWeight));
            joinObjFunction.clear();
            bmoEncodings.get(posWeight).incUpdateCardinality(this.solver, joinObjFunction, this.objFunction, weights.get(posWeight), encodingAssumptions);
            firstEncoding.set(posWeight, false);
          }
        } else
          bmoEncodings.get(posWeight).incUpdateCardinality(this.solver, joinObjFunction, this.objFunction, weights.get(posWeight), encodingAssumptions);
        for (int i = 0; i < encodingAssumptions.size(); i++)
          this.assumptions.push(encodingAssumptions.get(i));
      }
    }
  }

  private MiniSatStyleSolver rebuildSolver() {
    final MiniSatStyleSolver s = newSATSolver();
    for (int i = 0; i < nVars(); i++)
      newSATVariable(s);
    for (int i = 0; i < nHard(); i++)
      s.addClause(hardClauses.get(i).clause(), null);
    LNGIntVector clause;
    for (int i = 0; i < nSoft(); i++) {
      clause = new LNGIntVector(softClauses.get(i).clause());
      for (int j = 0; j < softClauses.get(i).relaxationVars().size(); j++)
        clause.push(softClauses.get(i).relaxationVars().get(j));
      s.addClause(clause, null);
    }
    return s;
  }

  private void initRelaxation() {
    for (int i = 0; i < nbSoft; i++) {
      final int l = newLiteral(false);
      softClauses.get(i).relaxationVars().push(l);
      softClauses.get(i).setAssumptionVar(l);
    }
  }

  @Override
  public String toString() {
    return this.getClass().getSimpleName();
  }
}
