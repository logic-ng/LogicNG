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

package org.logicng.solvers;

import org.logicng.collections.LNGBooleanVector;
import org.logicng.collections.LNGIntVector;
import org.logicng.datastructures.Assignment;
import org.logicng.formulas.Formula;
import org.logicng.formulas.Literal;
import org.logicng.formulas.Variable;
import org.logicng.handlers.MaxSATHandler;
import org.logicng.solvers.maxsat.algorithms.IncWBO;
import org.logicng.solvers.maxsat.algorithms.LinearSU;
import org.logicng.solvers.maxsat.algorithms.LinearUS;
import org.logicng.solvers.maxsat.algorithms.MSU3;
import org.logicng.solvers.maxsat.algorithms.MaxSAT;
import org.logicng.solvers.maxsat.algorithms.MaxSATConfig;
import org.logicng.solvers.maxsat.algorithms.WBO;
import org.logicng.solvers.maxsat.algorithms.WMSU3;

import java.util.SortedMap;
import java.util.TreeMap;

import static org.logicng.solvers.maxsat.algorithms.MaxSAT.MaxSATResult.OPTIMUM;
import static org.logicng.solvers.maxsat.algorithms.MaxSAT.MaxSATResult.UNDEF;
import static org.logicng.solvers.maxsat.algorithms.MaxSAT.MaxSATResult.UNSATISFIABLE;

/**
 * A wrapper for the OpenWBO solver.
 * @version 1.3
 * @since 1.0
 */
public final class MaxSATSolver {

  private enum Algorithm {WBO, INC_WBO, LINEAR_SU, LINEAR_US, MSU3, WMSU3}

  private final MaxSATConfig configuration;
  private final Algorithm algorithm;
  private MaxSAT.MaxSATResult result;
  private MaxSAT solver;
  private SortedMap<Variable, Integer> var2index;
  private SortedMap<Integer, Variable> index2var;

  /**
   * Constructs a new MaxSAT solver with a given configuration.
   * @param configuration the configuration
   * @param algorithm     the algorithm
   * @throws IllegalArgumentException if the algorithm was unknown
   */
  private MaxSATSolver(final MaxSATConfig configuration, final Algorithm algorithm) {
    this.algorithm = algorithm;
    this.configuration = configuration;
    this.reset();
  }

  /**
   * Returns a new MaxSAT solver using incremental WBO as algorithm with the default configuration.
   * @return the MaxSAT solver
   */
  public static MaxSATSolver incWBO() {
    return new MaxSATSolver(new MaxSATConfig.Builder().build(), Algorithm.INC_WBO);
  }

  /**
   * Returns a new MaxSAT solver using incremental WBO as algorithm with the given configuration.
   * @param config the configuration
   * @return the MaxSAT solver
   */
  public static MaxSATSolver incWBO(final MaxSATConfig config) {
    return new MaxSATSolver(config, Algorithm.INC_WBO);
  }

  /**
   * Returns a new MaxSAT solver using LinearSU as algorithm with the default configuration.
   * @return the MaxSAT solver
   */
  public static MaxSATSolver linearSU() {
    return new MaxSATSolver(new MaxSATConfig.Builder().cardinality(MaxSATConfig.CardinalityEncoding.MTOTALIZER).build(), Algorithm.LINEAR_SU);
  }

  /**
   * Returns a new MaxSAT solver using LinearSU as algorithm with the given configuration.
   * @param config the configuration
   * @return the MaxSAT solver
   */
  public static MaxSATSolver linearSU(final MaxSATConfig config) {
    return new MaxSATSolver(config, Algorithm.LINEAR_SU);
  }

  /**
   * Returns a new MaxSAT solver using LinearUS as algorithm with the default configuration.
   * @return the MaxSAT solver
   */
  public static MaxSATSolver linearUS() {
    return new MaxSATSolver(new MaxSATConfig.Builder().build(), Algorithm.LINEAR_US);
  }

  /**
   * Returns a new MaxSAT solver using LinearUS as algorithm with the given configuration.
   * @param config the configuration
   * @return the MaxSAT solver
   */
  public static MaxSATSolver linearUS(final MaxSATConfig config) {
    return new MaxSATSolver(config, Algorithm.LINEAR_US);
  }

  /**
   * Returns a new MaxSAT solver using MSU3 as algorithm with the default configuration.
   * @return the MaxSAT solver
   */
  public static MaxSATSolver msu3() {
    return new MaxSATSolver(new MaxSATConfig.Builder().build(), Algorithm.MSU3);
  }

  /**
   * Returns a new MaxSAT solver using MSU3 as algorithm with the given configuration.
   * @param config the configuration
   * @return the MaxSAT solver
   */
  public static MaxSATSolver msu3(final MaxSATConfig config) {
    return new MaxSATSolver(config, Algorithm.MSU3);
  }

  /**
   * Returns a new MaxSAT solver using WBO as algorithm with the default configuration.
   * @return the MaxSAT solver
   */
  public static MaxSATSolver wbo() {
    return new MaxSATSolver(new MaxSATConfig.Builder().build(), Algorithm.WBO);
  }

  /**
   * Returns a new MaxSAT solver using MSU3 as algorithm with the given configuration.
   * @param config the configuration
   * @return the MaxSAT solver
   */
  public static MaxSATSolver wbo(final MaxSATConfig config) {
    return new MaxSATSolver(config, Algorithm.WBO);
  }

  /**
   * Returns a new MaxSAT solver using weighted MSU3 as algorithm with the default configuration.
   * @return the MaxSAT solver
   */
  public static MaxSATSolver wmsu3() {
    return new MaxSATSolver(new MaxSATConfig.Builder().incremental(MaxSATConfig.IncrementalStrategy.ITERATIVE).build(), Algorithm.WMSU3);
  }

  /**
   * Returns a new MaxSAT solver using weighted MSU3 as algorithm with the given configuration.
   * @param config the configuration
   * @return the MaxSAT solver
   */
  public static MaxSATSolver wmsu3(final MaxSATConfig config) {
    return new MaxSATSolver(config, Algorithm.WMSU3);
  }

  /**
   * Resets the solver.
   * @throws IllegalArgumentException if the algorithm was unknown
   */
  public void reset() {
    this.result = UNDEF;
    this.var2index = new TreeMap<>();
    this.index2var = new TreeMap<>();
    switch (this.algorithm) {
      case WBO:
        this.solver = new WBO(this.configuration);
        break;
      case INC_WBO:
        this.solver = new IncWBO(this.configuration);
        break;
      case LINEAR_SU:
        this.solver = new LinearSU(this.configuration);
        break;
      case LINEAR_US:
        this.solver = new LinearUS(this.configuration);
        break;
      case MSU3:
        this.solver = new MSU3(this.configuration);
        break;
      case WMSU3:
        this.solver = new WMSU3(this.configuration);
        break;
      default:
        throw new IllegalArgumentException("Unknown MaxSAT algorithm: " + this.algorithm);
    }
  }

  /**
   * Adds a new hard formula to the solver.  Hard formulas must always be true.
   * @param formula the formula
   * @throws IllegalStateException if a formula is added to a solver which is already solved.
   */
  public void addHardFormula(final Formula formula) {
    if (this.result != UNDEF)
      throw new IllegalStateException("The MaxSAT solver does currently not support an incremental interface.  Reset the solver.");
    this.addCNF(formula.cnf(), -1);
  }

  /**
   * Adds a new soft formula to the solver.
   * @param formula the formula
   * @param weight  the weight
   * @throws IllegalStateException    if a formula is added to a solver which is already solved.
   * @throws IllegalArgumentException if the weight is &lt;1
   */
  public void addSoftFormula(final Formula formula, int weight) {
    if (this.result != UNDEF)
      throw new IllegalStateException("The MaxSAT solver does currently not support an incremental interface.  Reset the solver.");
    if (weight < 1)
      throw new IllegalArgumentException("The weight of a formula must be > 0");
    this.addCNF(formula.cnf(), weight);
  }

  /**
   * Adds a formula which is already in CNF to the solver.
   * @param formula the formula in CNF
   * @param weight  the weight of this CNF (or -1 for a hard constraint)
   */
  private void addCNF(final Formula formula, int weight) {
    switch (formula.type()) {
      case TRUE:
        break;
      case FALSE:
      case LITERAL:
      case OR:
        this.addClause(formula, weight);
        break;
      case AND:
        for (Formula op : formula)
          this.addClause(op, weight);
        break;
      default:
        throw new IllegalArgumentException("Input formula ist not a valid CNF: " + formula);
    }
  }

  /**
   * Adds a clause to the solver.
   * @param formula the clause
   * @param weight  the weight of the clause (or -1 for a hard clause)
   */
  private void addClause(final Formula formula, int weight) {
    this.result = UNDEF;
    final LNGIntVector clauseVec = new LNGIntVector((int) formula.numberOfAtoms());
    for (Literal lit : formula.literals()) {
      Integer index = this.var2index.get(lit.variable());
      if (index == null) {
        index = this.solver.newLiteral(false) >> 1;
        this.var2index.put(lit.variable(), index);
        this.index2var.put(index, lit.variable());
      }
      int litNum = lit.phase() ? index * 2 : (index * 2) ^ 1;
      clauseVec.push(litNum);
    }
    if (weight == -1) {
      this.solver.addHardClause(clauseVec);
    } else {
      this.solver.setCurrentWeight(weight);
      this.solver.updateSumWeights(weight);
      this.solver.addSoftClause(weight, clauseVec);
    }
  }

  /**
   * Solves the formula on the solver and returns the result.
   * @return the result (SAT, UNSAT, Optimum found)
   */
  public MaxSAT.MaxSATResult solve() {
    return solve(null);
  }

  /**
   * Solves the formula on the solver and returns the result.
   * @param handler a MaxSAT handler
   * @return the result (SAT, UNSAT, Optimum found, or UNDEF if canceled by the handler)
   */
  public MaxSAT.MaxSATResult solve(final MaxSATHandler handler) {
    if (this.result != UNDEF)
      return this.result;
    if (this.solver.currentWeight() == 1)
      this.solver.setProblemType(MaxSAT.ProblemType.UNWEIGHTED);
    else
      this.solver.setProblemType(MaxSAT.ProblemType.WEIGHTED);
    this.result = this.solver.search(handler);
    return this.result;
  }

  /**
   * Returns the minimum weight (or number of clauses if unweighted) of clauses which have to be unsatisfied.
   * Therefore, if the minimum number of weights is 0, the formula is satisfiable.
   * @return the minimum weight of clauses which have to be unsatisfied
   * @throws IllegalStateException if the formula is not yet solved
   */
  public int result() {
    if (this.result == UNDEF)
      throw new IllegalStateException("Cannot get a result as long as the formula is not solved.  Call 'solve' first.");
    return this.result == OPTIMUM ? this.solver.result() : -1;
  }

  /**
   * Returns the model of the current result.
   * @return the model of the current result
   * @throws IllegalStateException if the formula is not yet solved
   */
  public Assignment model() {
    if (this.result == UNDEF)
      throw new IllegalStateException("Cannot get a model as long as the formula is not solved.  Call 'solve' first.");
    return this.result != UNSATISFIABLE ? this.createAssignment(this.solver.model()) : null;
  }

  /**
   * Creates an assignment from a Boolean vector of the solver.
   * @param vec the vector of the solver
   * @return the assignment
   */
  private Assignment createAssignment(final LNGBooleanVector vec) {
    final Assignment model = new Assignment();
    for (int i = 0; i < vec.size(); i++) {
      final Literal lit = this.index2var.get(i);
      if (lit != null) {
        if (vec.get(i))
          model.addLiteral(lit);
        else
          model.addLiteral(lit.negate());
      }
    }
    return model;
  }

  /**
   * Returns the stats of the underlying solver.
   * @return the stats of the underlying solver
   */
  public MaxSAT.Stats stats() {
    return this.solver.stats();
  }

  @Override
  public String toString() {
    return String.format("MaxSATSolver{result=%s, var2index=%s}", this.result, this.var2index);
  }
}
